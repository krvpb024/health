-- DropIndex
DROP INDEX "session_account_id_key";

-- AlterTable
ALTER TABLE "session" ALTER COLUMN "expire_at" SET DEFAULT now() + INTERVAL '31 days';
