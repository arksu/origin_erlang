<?php
require_once 'db_work.php';
require_once 'utils.php';
db_connect();

$login = 1;
$id=307;
while ($login < 31) {
//$q = 'INSERT INTO `account` (`login`,`pass`,`email`,`status`) VALUES ("mrWork'.$login.'","q2wgfh5","exp.byte@gmail.com",1)';

$q = 'INSERT INTO `char` (`account_id`,`name`,`x`,`y`) VALUES ("'.($id+$login).'", "mrWork'.$login.'", "3000", "3000")';

db_query($q);

$login++;
}

?>
