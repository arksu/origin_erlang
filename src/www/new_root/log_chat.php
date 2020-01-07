<head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
</head>
<body>
<?php
require_once 'utils.php';

mysql_connect("localhost", "root", "004582");
mysql_select_db("a1");
mysql_query("SET NAMES utf8");
mysql_query("SET CHARSET utf8");

$q = 'SELECT `log_chat`.`msg`, `log_chat`.`nick`, `log_chat`.`time`, `channel`, `char`.`name` FROM `log_chat`
    left join  `char` on `char`.`id` = `log_chat`.`char_id`
    ORDER BY `log_chat`.`id`  DESC
    limit 0, 150';
$res = mysql_query($q);

echo '<table border="0">';
while ($row = mysql_fetch_array($res)) {

    echo '<tr><td>';
    echo $row['time'].'</td><td><b><u>'.$row['name'].'</u></b></td><td> ['.$row['channel'].'] : ['.$row['nick'].']</td><td> '.$row['msg'];

    echo '</td></tr>';
}
echo '</table>';
?>