defmodule Carbonite.TestRepo.Migrations.UpdateCarboniteToV6 do
  use Ecto.Migration

  def up do
    Carbonite.Migrations.up(6, carbonite_prefix: "alternate_test_schema")
  end

  def up do
    Carbonite.Migrations.down(5, carbonite_prefix: "alternate_test_schema")
  end
end
