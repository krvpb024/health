-- AlterTable
ALTER TABLE "session" ALTER COLUMN "expire_at" SET DEFAULT now() + INTERVAL '31 days';

-- CreateTable
CREATE TABLE "session_message" (
    "id" TEXT NOT NULL,
    "session_id" TEXT NOT NULL,
    "content" TEXT NOT NULL,
    "expire_at" TIMESTAMPTZ NOT NULL DEFAULT now() + INTERVAL '5 minutes',

    CONSTRAINT "session_message_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "profile" (
    "id" SERIAL NOT NULL,
    "account_id" INTEGER NOT NULL,
    "gender" BOOLEAN NOT NULL,
    "birthday" DATE NOT NULL,
    "init_height" INTEGER NOT NULL,

    CONSTRAINT "profile_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "profile_account_id_key" ON "profile"("account_id");

-- AddForeignKey
ALTER TABLE "session_message" ADD CONSTRAINT "session_message_session_id_fkey" FOREIGN KEY ("session_id") REFERENCES "session"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "profile" ADD CONSTRAINT "profile_account_id_fkey" FOREIGN KEY ("account_id") REFERENCES "account"("id") ON DELETE CASCADE ON UPDATE CASCADE;
