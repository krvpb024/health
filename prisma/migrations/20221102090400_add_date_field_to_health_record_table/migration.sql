/*
  Warnings:

  - You are about to alter the column `weight` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `Unsupported("NUMERIC")`.
  - You are about to alter the column `body_fat_percentage` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `Unsupported("NUMERIC")`.
  - You are about to alter the column `waistline_cm` on the `health_record` table. The data in that column could be lost. The data in that column will be cast from `Decimal` to `Unsupported("NUMERIC")`.
  - You are about to drop the `session_message` table. If the table is not empty, all the data it contains will be lost.

*/
-- DropForeignKey
ALTER TABLE "session_message" DROP CONSTRAINT "session_message_session_id_fkey";

-- AlterTable
ALTER TABLE "health_record" ADD COLUMN     "date" DATE NOT NULL DEFAULT CURRENT_TIMESTAMP,
ALTER COLUMN "weight" SET DATA TYPE NUMERIC,
ALTER COLUMN "body_fat_percentage" SET DATA TYPE NUMERIC,
ALTER COLUMN "waistline_cm" SET DATA TYPE NUMERIC;

-- DropTable
DROP TABLE "session_message";
