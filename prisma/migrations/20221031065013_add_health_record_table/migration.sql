-- AlterTable
ALTER TABLE "session" ALTER COLUMN "expire_at" SET DEFAULT now() + INTERVAL '31 days';

-- AlterTable
ALTER TABLE "session_message" ALTER COLUMN "expire_at" SET DEFAULT now() + INTERVAL '5 minutes';

-- CreateTable
CREATE TABLE "health_record" (
    "id" SERIAL NOT NULL,
    "profile_id" INTEGER NOT NULL,
    "height" INTEGER NOT NULL,
    "weight" DOUBLE PRECISION NOT NULL,
    "body_fat_percentage" DOUBLE PRECISION,
    "waistline_cm" DOUBLE PRECISION,

    CONSTRAINT "health_record_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "health_record_profile_id_key" ON "health_record"("profile_id");

-- AddForeignKey
ALTER TABLE "health_record" ADD CONSTRAINT "health_record_profile_id_fkey" FOREIGN KEY ("profile_id") REFERENCES "profile"("id") ON DELETE CASCADE ON UPDATE CASCADE;
