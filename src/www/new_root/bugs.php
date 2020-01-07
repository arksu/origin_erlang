<head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
</head>
<body>
<?php
mysql_connect("localhost", "root", "004582");
mysql_select_db("a1");
mysql_query("SET NAMES utf8");
mysql_query("SET CHARSET utf8");

$res = mysql_query("select * from `bug_report` left join `account` on `account`.`id` = `bug_report`.`acc_id` order by `bug_report`.`id` desc");
echo "rows=".mysql_num_rows($res)."<br>";
while ($row = mysql_fetch_array($res)) {
    echo $row['time']." : <b><u>".
    $row['login'].
    "</u></b> : ".
    $row['subj'].
    "  :  ".
    $row['text'].
    "<br>";
}

echo "ok";

?>
</body>