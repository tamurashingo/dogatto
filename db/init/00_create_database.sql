-- Initialize DOGATTO database
-- This script runs automatically when MySQL container starts

CREATE DATABASE IF NOT EXISTS dogatto_development CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
CREATE DATABASE IF NOT EXISTS dogatto_test CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

GRANT ALL PRIVILEGES ON dogatto_development.* TO 'dogatto'@'%';
GRANT ALL PRIVILEGES ON dogatto_test.* TO 'dogatto'@'%';

FLUSH PRIVILEGES;
