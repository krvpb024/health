/*
  Warnings:

  - You are about to alter the column `height` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Integer` to `Unsupported("NUMERIC")`.
  - You are about to alter the column `weight` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `Unsupported("NUMERIC")`.
  - You are about to alter the column `body_fat_percentage` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `Unsupported("NUMERIC")`.
  - You are about to alter the column `waistline_cm` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `Unsupported("NUMERIC")`.
  - You are about to alter the column `init_height` on the `profile` table. The data in that column could be lost. The data in that column will be cast from `Integer` to `Unsupported("NUMERIC")`.

*/
-- AlterTable
ALTER TABLE "health_record" ALTER COLUMN "height" SET DATA TYPE NUMERIC,
ALTER COLUMN "weight" SET DATA TYPE NUMERIC,
ALTER COLUMN "body_fat_percentage" SET DATA TYPE NUMERIC,
ALTER COLUMN "waistline_cm" SET DATA TYPE NUMERIC;

-- AlterTable
ALTER TABLE "profile" ALTER COLUMN "init_height" SET DATA TYPE NUMERIC;
