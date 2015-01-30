CREATE DATABASE swiftonsnap;
GRANT ALL PRIVILEGES on DATABASE swiftonsnap TO postgres;
CREATE TABLE users(id SERIAL, device_token VARCHAR(255));
