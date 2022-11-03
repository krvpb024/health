/*
  Warnings:

  - You are about to alter the column `height` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `DoublePrecision`.
  - You are about to alter the column `weight` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `DoublePrecision`.
  - You are about to alter the column `body_fat_percentage` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `DoublePrecision`.
  - You are about to alter the column `waistline_cm` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `DoublePrecision`.
  - You are about to drop the column `init_height` on the `profile` table. All the data in the column will be lost.
  - Added the required column `height` to the `profile` table without a default value. This is not possible if the table is not empty.

*/
-- AlterTable
ALTER TABLE "health_record" ALTER COLUMN "height" SET DATA TYPE DOUBLE PRECISION,
ALTER COLUMN "weight" SET DATA TYPE DOUBLE PRECISION,
ALTER COLUMN "body_fat_percentage" SET DATA TYPE DOUBLE PRECISION,
ALTER COLUMN "waistline_cm" SET DATA TYPE DOUBLE PRECISION;

-- AlterTable
ALTER TABLE "profile" RENAME COLUMN "init_height" TO "height";
ALTER TABLE "profile" ALTER COLUMN "height" SET DATA TYPE DOUBLE PRECISION;

