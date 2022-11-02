/*
  Warnings:

  - You are about to alter the column `weight` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `Unsupported("NUMERIC")`.
  - You are about to alter the column `body_fat_percentage` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `Unsupported("NUMERIC")`.
  - You are about to alter the column `waistline_cm` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `Unsupported("NUMERIC")`.

*/
-- DropIndex
DROP INDEX "health_record_profile_id_key";

-- AlterTable
ALTER TABLE "health_record" ALTER COLUMN "weight" SET DATA TYPE NUMERIC,
ALTER COLUMN "body_fat_percentage" SET DATA TYPE NUMERIC,
ALTER COLUMN "waistline_cm" SET DATA TYPE NUMERIC;

-- AlterTable
ALTER TABLE "session" ALTER COLUMN "expire_at" SET DEFAULT now() + INTERVAL '31 days';

-- AlterTable
ALTER TABLE "session_message" ALTER COLUMN "expire_at" SET DEFAULT now() + INTERVAL '5 minutes';
