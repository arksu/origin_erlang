<?php
require_once 'db_work.php';
require_once 'utils.php';

	db_connect();
	// ищем юзера с таким же логином
	$q = 'SELECT `login`,`email`,`id` FROM `account`';
	$res = db_query($q);
	
	while ($arr = mysql_fetch_array($res)) {
		$login = strtolower($arr['login']);
		$email = strtolower($arr['email']);
		$q = 'UPDATE `account` SET `login`="'.$login.'", `email`="'.$email.'" WHERE `id`="'.$arr['id'].'"';
		echo "login=".$arr['login']." email=".$arr['email']."  =>  login=".$login." email=".$email;
		db_query($q);
	}
?>