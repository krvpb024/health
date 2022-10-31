/*
  Warnings:

  - You are about to drop the column `birthday` on the `profile` table. All the data in the column will be lost.
  - Added the required column `birth_date` to the `profile` table without a default value. This is not possible if the table is not empty.

*/
-- AlterTable
ALTER TABLE "profile" DROP COLUMN "birthday",
ADD COLUMN     "birth_date" DATE NOT NULL;

-- AlterTable
ALTER TABLE "session" ALTER COLUMN "expire_at" SET DEFAULT now() + INTERVAL '31 days';

-- AlterTable
ALTER TABLE "session_message" ALTER COLUMN "expire_at" SET DEFAULT now() + INTERVAL '5 minutes';
