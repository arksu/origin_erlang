GRANT USAGE ON *.* TO 'a1_online'@'localhost' IDENTIFIED BY PASSWORD '*SOME';

GRANT SELECT, INSERT, UPDATE ON `a1`.`account` TO 'a1_online'@'localhost';

GRANT SELECT ON `a1`.`server` TO 'a1_online'@'localhost';

GRANT SELECT (id, x, name, account_id, y), INSERT (id, x, name, account_id, y) ON `a1`.`char` TO 'a1_online'@'localhost';




GRANT USAGE ON *.* TO 'phpbb'@'localhost' IDENTIFIED BY PASSWORD '*SOME';

GRANT ALL PRIVILEGES ON `phpbb`.* TO 'phpbb'@'localhost' WITH GRANT OPTION;

