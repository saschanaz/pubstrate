CREATE TABLE activities (
       id BIGSERIAL PRIMARY KEY,
       data JSONB NOT NULL
            -- Make sure we have a valid ID
            CONSTRAINT activity_must_have_id_and_type
            CHECK (data ? '@id' AND data ? '@type'));
