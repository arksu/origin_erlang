<?php
include('config.php');
global $sql_time;
global $sql_qnum;
$sql_qnumm = 0;
$sql_time = 0;
function db_connect()
{
	global $cfg;
	global $db_link;
	$db_link = mysql_connect($cfg['db_host'],$cfg['db_user'],$cfg['db_pass']) OR DIE("error=Не могу создать соединение mysql");
	mysql_select_db($cfg['db_name'], $db_link) or die('error='.mysql_error());

	global $sql_time;
	$mtime = microtime(); 
	$mtime = explode(" ",$mtime); 
	$mtime = $mtime[1] + $mtime[0]; 
	$tstart = $mtime;

//	mysql_query("SET NAMES koi8r", $db_link);
//	mysql_query("SET CHARSET koi8r", $db_link);
//	mysql_query("SET collation_connection = koi8r_general_ci", $db_link);
//	mysql_query("SET collation_database = koi8r_general_ci", $db_link);
//	mysql_query("SET collation_database = koi8r_general_ci", $db_link);
//	mysql_query("SET character_set_client = koi8r", $db_link);
//	mysql_query("SET character_set_connection = koi8r", $db_link);
//	mysql_query("SET character_set_database = koi8r", $db_link);
//	mysql_query("SET character_set_results = koi8r", $db_link);
	
	$mtime = microtime(); 
	$mtime = explode(" ",$mtime); 
	$mtime = $mtime[1] + $mtime[0]; 
	$tend = $mtime;
	$sql_time += ($tend - $tstart);
}
function db_query($q)
{
	global $db_link;
	global $sql_time;
	global $sql_qnum;
	$mtime = microtime(); 
	$mtime = explode(" ",$mtime); 
	$mtime = $mtime[1] + $mtime[0]; 
	$tstart = $mtime;
	
	$sql_qnum++;
	$result =  mysql_query($q, $db_link);
	
	$mtime = microtime(); 
	$mtime = explode(" ",$mtime); 
	$mtime = $mtime[1] + $mtime[0]; 
	$tend = $mtime;
	$sql_time += ($tend - $tstart);
	return $result;
}
function db_error()
{
	global $db_link;
	return mysql_error($db_link);
}
function db_num_rows($res)
{
	return mysql_num_rows($res);
}
function db_fetch_array($res)
{
	return mysql_fetch_array($res);
}
function db_data_seek($res, $pos)
{
	return mysql_data_seek($res, $pos);
}
function db_close()
{
	global $db_link;
	mysql_close($db_link);
}
?>