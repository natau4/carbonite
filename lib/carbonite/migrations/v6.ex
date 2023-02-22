defmodule Carbonite.Migrations.V6 do
  @moduledoc false

  use Ecto.Migration
  use Carbonite.Migrations.Version

  alias Carbonite.Migrations.V5

  @type prefix :: binary()

  @type up_option :: {:carbonite_prefix, prefix()}

  @spec create_skip_transaction_procedure(prefix()) :: :ok
  def create_skip_transaction_procedure(prefix) do
    """
    CREATE OR REPLACE FUNCTION #{prefix}.skip_transaction() RETURNS TRIGGER AS
    $body$
    BEGIN
      IF (NEW.meta->>'skip' IS NOT NULL) THEN
        RETURN NULL;
      ELSE
        RETURN NEW;
      END IF;
    END
    $body$
    LANGUAGE plpgsql;
    """
    |> squish_and_execute()

    :ok
  end

  @spec create_capture_changes_procedure(prefix()) :: :ok
  def create_capture_changes_procedure(prefix) do
    """
    CREATE OR REPLACE FUNCTION #{prefix}.capture_changes() RETURNS TRIGGER AS
    $body$
    DECLARE
      trigger_row #{prefix}.triggers;
      change_row #{prefix}.changes;
      pk_source RECORD;
      col_name VARCHAR;
      pk_col_val VARCHAR;
      old_field RECORD;
      old_field_jsonb JSONB;
    BEGIN
      /* load trigger config */
      SELECT *
        INTO trigger_row
        FROM #{prefix}.triggers
        WHERE table_prefix = TG_TABLE_SCHEMA AND table_name = TG_TABLE_NAME;

      IF
        (trigger_row.mode = 'ignore' AND (trigger_row.override_xact_id IS NULL OR trigger_row.override_xact_id != pg_current_xact_id())) OR
        (trigger_row.mode = 'capture' AND trigger_row.override_xact_id = pg_current_xact_id())
      THEN
        RETURN NULL;
      END IF;

      /* instantiate change row */
      change_row = ROW(
        NEXTVAL('#{prefix}.changes_id_seq'),
        pg_current_xact_id(),
        LOWER(TG_OP::TEXT),
        TG_TABLE_SCHEMA::TEXT,
        TG_TABLE_NAME::TEXT,
        NULL,
        NULL,
        '{}',
        NULL,
        NULL
      );

      /* build table_pk */
      IF trigger_row.primary_key_columns != '{}' THEN
        IF (TG_OP IN ('INSERT', 'UPDATE')) THEN
          pk_source := NEW;
        ELSIF (TG_OP = 'DELETE') THEN
          pk_source := OLD;
        END IF;

        change_row.table_pk := '{}';

        FOREACH col_name IN ARRAY trigger_row.primary_key_columns LOOP
          EXECUTE 'SELECT $1.' || col_name || '::TEXT' USING pk_source INTO pk_col_val;
          change_row.table_pk := change_row.table_pk || pk_col_val;
        END LOOP;
      END IF;

      /* fill in changed data */
      IF (TG_OP = 'UPDATE') THEN
        change_row.data = to_jsonb(NEW.*) - trigger_row.excluded_columns;
        change_row.changed_from = '{}'::JSONB;

        FOR old_field_jsonb
        IN SELECT jsonb_build_object(key, value)
        FROM jsonb_each(to_jsonb(OLD.*) - trigger_row.excluded_columns)
        LOOP
          IF NOT change_row.data @> old_field_jsonb THEN
            change_row.changed_from := change_row.changed_from || old_field_jsonb;
          END IF;
        END LOOP;

        change_row.changed := ARRAY(SELECT jsonb_object_keys(change_row.changed_from));

        IF change_row.changed = '{}' THEN
          /* All changed fields are ignored. Skip this update. */
          RETURN NULL;
        END IF;

        /* Persisting the old data is opt-in, discard if not configured. */
        IF trigger_row.store_changed_from IS FALSE THEN
          change_row.changed_from := NULL;
        END IF;
      ELSIF (TG_OP = 'DELETE') THEN
        change_row.data = to_jsonb(OLD.*) - trigger_row.excluded_columns;
      ELSIF (TG_OP = 'INSERT') THEN
        change_row.data = to_jsonb(NEW.*) - trigger_row.excluded_columns;
      END IF;

      /* filtered columns */
      FOREACH col_name IN ARRAY trigger_row.filtered_columns LOOP
        change_row.data = jsonb_set(change_row.data, ('{' || col_name || '}')::TEXT[], jsonb('"[FILTERED]"'));
      END LOOP;

      /* insert, fail gracefully unless transaction record present or NEXTVAL has never been called */
      BEGIN
        change_row.transaction_id = CURRVAL('#{prefix}.transactions_id_seq');

        /* verify that xact_id matches */
        IF NOT
          EXISTS(
            SELECT 1 FROM #{prefix}.transactions
            WHERE xact_id = change_row.transaction_xact_id
          )
        THEN
          RETURN NULL;
        END IF;

        INSERT INTO #{prefix}.changes VALUES (change_row.*);

        EXCEPTION WHEN object_not_in_prerequisite_state THEN
          RETURN NULL;
      END;

      RETURN NULL;
    END;
    $body$
    LANGUAGE plpgsql;
    """
    |> squish_and_execute()

    :ok
  end

  def up(opts) do
    prefix = Keyword.get(opts, :carbonite_prefix, default_prefix())

    create_skip_transaction_procedure(prefix)
    create_capture_changes_procedure(prefix)

    """
    CREATE TRIGGER skip_transaction_trigger
    BEFORE INSERT
    ON #{prefix}.transactions
    FOR EACH ROW
    EXECUTE PROCEDURE #{prefix}.skip_transaction();
    """
    |> squish_and_execute()
  end

  def down(opts) when is_list(opts) do
    prefix = Keyword.get(opts, :carbonite_prefix, default_prefix())

    execute("DROP TRIGGER skip_transaction_trigger ON #{prefix}.transactions;")
    execute("DROP FUNCTION #{prefix}.skip_transaction;")

    V5.create_capture_changes_procedure(prefix)
  end
end
