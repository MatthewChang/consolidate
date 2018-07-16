DROP SCHEMA public CASCADE;
CREATE SCHEMA public;

CREATE TABLE songs (
    id SERIAL PRIMARY KEY,
    name varchar(255)
);

CREATE TABLE tags (
    id SERIAL PRIMARY KEY ,
    name varchar(255)
);

CREATE TABLE songTags (
    id SERIAL PRIMARY KEY,
    songId int REFERENCES songs ON DELETE CASCADE,
    tagId int REFERENCES tags ON DELETE CASCADE
);
CREATE UNIQUE INDEX songTagsIds ON songTags (songId,tagId);
