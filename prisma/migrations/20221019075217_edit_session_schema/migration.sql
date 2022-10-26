/*
  Warnings:

  - A unique constraint covering the columns `[account_id]` on the table `session` will be added. If there are existing duplicate values, this will fail.
  - Added the required column `account_id` to the `session` table without a default value. This is not possible if the table is not empty.

*/
-- AlterTable
ALTER TABLE "session" ADD COLUMN     "account_id" INTEGER NOT NULL,
ALTER COLUMN "expire_at" SET DEFAULT now() + INTERVAL '31 days';

-- CreateIndex
CREATE UNIQUE INDEX "session_account_id_key" ON "session"("account_id");

-- AddForeignKey
ALTER TABLE "session" ADD CONSTRAINT "session_account_id_fkey" FOREIGN KEY ("account_id") REFERENCES "account"("id") ON DELETE CASCADE ON UPDATE CASCADE;
