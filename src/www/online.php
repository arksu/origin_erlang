<?php
function get_online() {
	$password = "a1_online";
	$username = "a1_online";
	$link = mysql_connect("localhost",$username,$password) OR DIE("Не могу создать соединение ");
	mysql_select_db("a1", $link) or die(mysql_error());
	
	$query = "SELECT val, UNIX_TIMESTAMP(time) as 'time' FROM server WHERE val_name = 'online_count'";
	$res = mysql_query($query, $link) or die(mysql_error());
	
	if (mysql_num_rows($res) <> 1) die ("error num rows");
	
	if  ($row = mysql_fetch_array($res)) {
		$a = array();
		$a['count'] = $row['val'];
		$a['time'] = $row['time'];
		return $a;
	} else return 0;
}
?>