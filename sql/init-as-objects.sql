-- The most minimal representation of activities in the system

CREATE TABLE as_objects (
       id BIGSERIAL PRIMARY KEY,
       data JSONB NOT NULL
            -- Make sure we have a valid ID
            CONSTRAINT as_objects_must_have_id_and_type
            CHECK (data ? '@id' AND data ? '@type'),
       -- Is this a local or a remote object?       
       local BOOLEAN NOT NULL,
       -- application-specific information about this object
       private JSONB);

CREATE UNIQUE INDEX as_object_id ON as_objects ((data->>'@id'));
