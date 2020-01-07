<?php
require_once 'db_work.php';
require_once 'utils.php';

db_connect();

$cur_month = date('n');
$cur_days = cal_days_in_month(CAL_GREGORIAN, $cur_month, date('Y'));

$next_month = $cur_month+1;
$nex_year = date('Y');
if ($next_month > 12) {
    $next_month = 1;
    $nex_year++;
}

$prev_month = $cur_month-1;
$prev_year = date('Y');
if ($prev_month < 0) {
    $prev_month = 12;
    $prev_year--;
}
$prev_days = cal_days_in_month(CAL_GREGORIAN, $prev_month, $prev_year);

echo 'period: '.date('Y').'-'.$cur_month.' ('.$cur_days.')  :  '.$prev_year.'-'.$prev_month.' ('.$prev_days.')';

$today = date('j');

echo '<table border="1"><tr><td>date</td><td>unique</td><td>total</td></tr>';
for ($d = $today; $d > 0; $d--) {
    echo '<tr>';
    $nm = $cur_month;
    $nd = $d+1;
    if ($d == $cur_days) {
        $nm = $next_month;
        $nd = 1;
    }

    $q = "SELECT count(distinct `account_id`) as c, count(`account_id`) as t FROM `log_login` where `time` between '".date('Y')."-".$cur_month."-".$d."'
    and '".date('Y')."-".$cur_month."-".($d+1)."' ";
    $res = db_query($q);
    echo '<td>'.date('Y')."-".$cur_month."-".$d.'</td>';
    if ($r = db_fetch_array($res)) {
        echo '<td>'.$r['c'].'</td>';
        echo '<td>'.$r['t'].'</td>';
    }

    echo '</tr>';
}
for ($d = $prev_days; $d > 0; $d--) {
    echo '<tr>';
    $nm = $prev_month;
    $nd = $d+1;
    if ($d == $prev_days) {
        $nm = $cur_month;
        $nd = 1;
    }
    $q = "SELECT count(distinct `account_id`) as c, count(`account_id`) as t FROM `log_login` where `time` between '".$prev_year."-".$prev_month."-".$d."'
    and '".$prev_year."-".$nm."-".($nd)."' ";
    $res = db_query($q);
    echo '<td>'.$prev_year."-".$prev_month."-".$d.'</td>';
    if ($r = db_fetch_array($res)) {
        echo '<td>'.$r['c'].'</td>';
        echo '<td>'.$r['t'].'</td>';
    }

    echo '</tr>';
}

echo '</table>';


?>