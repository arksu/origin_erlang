<?php
function check_param($p) {
	if ((strlen($p) > 32) || (strlen($p) == 0)) return false;
	
	for ($i = 0;$i<strlen($p);$i++) {
    	$o = ord(substr(strtoupper($p), $i, 1));
		if (($o < 48 || $o > 90) && $o != 95 && $o!=45 && $o != 46) return false;
	}
	return true;
}
function check_email($p) {
	if ((strlen($p) > 32) || (strlen($p) < 1)) return false;
	
	for ($i = 0;$i<strlen($p);$i++) {
    	$o = ord(substr(strtoupper($p), $i, 1));
		if (($o < 48 || $o > 90) && $o != 46 && $o != 45 && $o != 95) return false;
	}
	return true;
}
?>