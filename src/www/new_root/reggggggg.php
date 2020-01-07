<?php
require_once 'db_work.php';
require_once 'utils.php';
require_once "Mail.php";

define('IN_PHPBB', true);
$phpbb_root_path = (defined('PHPBB_ROOT_PATH')) ? PHPBB_ROOT_PATH : './forum/';
$phpEx = substr(strrchr(__FILE__, '.'), 1);
include($phpbb_root_path . 'common.' . $phpEx);
include($phpbb_root_path . 'includes/functions_user.' . $phpEx);

//-------------------------------------------------------------------------------------
function add_user_to_forum($user, $pass, $email) {


	// User's data
	$user_row['username'] = $user;
	$user_row['user_password'] = phpbb_hash($pass);
	$user_row['user_email'] = $email;
	$user_row['group_id'] = 2;
	$user_row['user_type'] = 0;
	// Adding user
	user_add($user_row, false);
	
	//echo 'done! user='.$user.' pwd='.$pass.' mail='.$email;
}
//-------------------------------------------------------------------------------------


if (isset($_POST['action']))
	$action = $_POST['action'];
else
	$action = $_GET['action'];
switch ($action) {
	case 'register' :
		register_user();
		break;
	case 'confirm' :
		register_confirm();
		break;
	case 'forum_add' :
		register_forum();
		break;
	default: 
		reg_form();
}

function register_forum() {
    $login = $_GET['login'];
    $pass = $_GET['pass'];
    $email = $_GET['email'];
    add_user_to_forum($login, $pass, $email);
}


//-------------------------------------------------------------------------------------
function reg_form() {
	echo 'register unavailable....';
/*
	echo '<form method=post>';
	echo "Login: <br>";
	echo '<input type="text" name="login" size="32" maxlength="32" /><br>';
	echo 'Password:<br>';
	echo '<input type="password" name="pass" size="32" maxlength="32" /><br>';
	echo "E-mail: <br>";
	echo '<input type="text" name="email" size="32" maxlength="32" /><br>';
	
	echo '<input name="action" type="hidden" value="register">';
	echo '<input type="submit" value="Register"><br>';
	echo '</form>';	
	*/
}

//-------------------------------------------------------------------------------------
function register_user() { echo 'register unavailable....'; };

function register_user_old() {
	if (!isset($_POST['login']) || !isset($_POST['pass']) || !isset($_POST['email'])) die ('havnt params!');
	$login = strtolower($_POST['login']);
	$pass = $_POST['pass'];
	$email = strtolower($_POST['email']);
	
	if (strlen($pass) < 4) die ("password too short! (min 4 symbols)");
	
	if (!check_param($login)) die ("incorrect login! check symbols");
	if (!check_param($pass)) die ("incorrect pass! check symbols");
	if (!check_email($email)) die ("incorrect email! check symbols");
	
	db_connect();
	// ищем юзера с таким же логином
	$q = 'SELECT `id` FROM `account` WHERE `login`="'.$login.'"';
	$res = db_query($q);
	if (db_num_rows($res) <> 0) die ("user already exist!");
	
	// ищем такое же мыло в базе
	$q = 'SELECT `id` FROM `account` WHERE `email`="'.$email.'"';
	$res = db_query($q);
	if (db_num_rows($res) <> 0) die ("email already exist!");
	
	
	// добавляем юзера
	$reg_id = md5($login.$pass);
	$q = 'INSERT INTO `account` (`login`,`pass`,`email`,`reg_id`,`status`) VALUES ("'.$login.'","'.$pass.'","'.$email.'","'.$reg_id.'",1)';
	db_query($q);
	
	
	echo "User registered! Please check your e-mail";
	// шлем мыло 
	$from = "Support of Origin <support@origin-world.com>";
 	$to = "<".$email.">";
 	$subject = "Please confirm your email";
 	$body = "Please confirm your registration by this link\r\n http://origin-world.com/reg.php?action=confirm&id=".$reg_id."\r\n The Origin-World.com developers team";
 
 	$host = "smtp.yandex.ru";
 	$username = "support@origin-world.com";
 	$password = "supportqwe";
 
 	$headers = array ('From' => $from,
	   'To' => $to,
	   'Subject' => $subject);
	$smtp = Mail::factory('smtp',
   	array ('host' => $host,
     	'auth' => true,
     	'username' => $username,
     	'password' => $password));
 
 	$mail = $smtp->send($to, $headers, $body);
 
 	if (PEAR::isError($mail)) {
   		echo("<p>" . $mail->getMessage() . "</p>");
  	} else {
   		echo("<p>Message successfully sent!</p>");
  	}
}

//-------------------------------------------------------------------------------------
function register_confirm() { echo 'register unavailable....'; };

function register_confirm_old() {
	if (!isset($_GET['id'])) die ('havnt params!');
	$id = $_GET['id'];
	if (!check_param($id)) die ("wrong id!");
	
	db_connect();
	$q = 'SELECT `id`, `login`,`email`,`pass` FROM `account` WHERE `reg_id`="'.$id.'" AND `status`=1';
	$res = db_query($q);
	if (db_num_rows($res) <> 1) die ("Registration not found!");
	// ���� ����� �����������. �� ���� �����������
	if  ($row = db_fetch_array($res)) {
		// ������ ������ � ����
		$q = 'UPDATE `account` SET `status`=0 WHERE `id`="'.$row['id'].'"';
		db_query($q);
		// ������� ����
		$q = 'INSERT INTO `char` (`account_id`,`name`,`x`,`y`) VALUES ("'.$row['id'].'", "'.$row['login'].'", "3000", "3000")';

		db_query($q);
		
		// ������� ����� �� �����
		add_user_to_forum($row['login'], $row['pass'], $row['email']);
		
		echo "Email confirmed!<br>Your first char ".$row['login']." was created. Enjoy play.<br>";
		echo "<a href=alpha.rar>Download game client</a><br>";
		echo "<a href=/forum/ >Forum</a> accaount was created! <br>";
		
		// ������ ������ � �������������� �����
	    
		$from = "Support of Origin <support@origin-world.com>";
	 	$to = "<".$email.">";
	 	$subject = "Email confirmed!";
	 	$body = "Your registration is confirmed! Enjoy play!\r\n The Origin-World.com developers team";
	 
	 	$host = "smtp.yandex.ru";
	 	$username = "support@origin-world.com";
	 	$password = "supportqwe";
	 
	 	$headers = array ('From' => $from,
		   'To' => $to,
		   'Subject' => $subject);
		$smtp = Mail::factory('smtp',
	   	array ('host' => $host,
	     	'auth' => true,
	     	'username' => $username,
	     	'password' => $password));
	 
	 	$mail = $smtp->send($to, $headers, $body);
	 
	 	if (PEAR::isError($mail)) {
	   		echo("<p>" . $mail->getMessage() . "</p>");
	  	} else {
	   		echo("<p>Message successfully sent!</p>");
	  	}
		
	}
}


?>