<?php
require_once 'db_work.php';
require_once 'utils.php';

db_connect();

$q = 'SELECT `log_login`.`time`, `log_login`.`login`, `result`, `account_id` FROM `log_login`
    left join  `account` on `account`.`id` = `log_login`.`account_id`
    ORDER BY `log_login`.`id`  DESC
    limit 0, 40';
$res = db_query($q);

while ($row = db_fetch_array($res)) {
    if ($row['result'] != 0) {
        $font = '<font color="red">';
        $font_end = '</font>';
    } else {
        $font = '<font color="black">';
        $font_end = '</font>';
    }
    echo $row['time'].' '.$font.'<b><u>'.$row['login'].'</u></b>'.$font_end.' res='.$row['result'];

    $q = 'SELECT `x`, `y`, `name` FROM `char` WHERE `account_id` = "'.$row['account_id'].'"';
    $res2 = db_query($q);
    while ($row2 = db_fetch_array($res2)) {
        echo '   ['.$row2['name'].' x='.$row2['x'].' y='.$row2['y'].']';
    }
    echo '<br>';
}

?>