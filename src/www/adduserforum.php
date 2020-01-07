<?php
define('IN_PHPBB', true);
$phpbb_root_path = (defined('PHPBB_ROOT_PATH')) ? PHPBB_ROOT_PATH : './forum/';
$phpEx = substr(strrchr(__FILE__, '.'), 1);
include($phpbb_root_path . 'common.' . $phpEx);
include($phpbb_root_path . 'includes/functions_user.' . $phpEx);

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

switch ($_GET['action']) {
	case 'add' :
		add_user_to_forum($_GET['login'], $_GET['pass'], $_GET['email']);
		break;
		
	default:
		
		break;
}


?>