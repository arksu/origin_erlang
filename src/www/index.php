<html>
<head>
<!--[if IE]>
        <link rel="stylesheet" type="text/css" href="ie.css" />
        <link href="jqModal.css" rel="stylesheet" type="text/css" />
<![endif]-->
<!--[if !IE]><!-->
        <link rel="stylesheet" href="master.css" type="text/css" media="screen" />
        <link href="jqModalne.css" rel="stylesheet" type="text/css" />
 <!--<![endif]-->
 		
		<script type="text/javascript" src="jquery-1.2.6.pack.js"></script>
		<script type="text/javascript" src="jqModal.js"></script>
		<script type="text/javascript" src="jqDnR.js"></script>
		<script type="text/javascript">
			function generatePreview()
			{
				var fname = $('#fname').val();
				var lname = $('#lname').val();
				var gender = $('input[@id=gender][@checked]').val();
				
				if (!gender)
				{
					gender = '';
				}
				

				preview = '<span style="font: 50px Eccentric Std, Arial, sans-serif;">';
				preview += '<form method=post action="reg.php">Login: <br><input type="text" name="login" size="32" maxlength="32" class="idle" onblur="this.className=\'idle\'" onfocus="this.className=\'activeField\'"/><br>Password:<br><input type="password" name="pass" size="32" maxlength="32" class="idle" onblur="this.className=\'idle\'" onfocus="this.className=\'activeField\'"/><br>E-mail: <br><input type="text" name="email" size="32" maxlength="32" class="idle" onblur="this.className=\'idle\'" onfocus="this.className=\'activeField\'"/><br><input name="action" type="hidden" value="register"><center><input type="submit" value="Register" class="button" onblur="this.className=\'button\'" onfocus="this.className=\'activeButton\'"></center><br></form>';
				preview += '</span>';
				
				return preview;
			}
			
			$(document).ready(function()
			{
				$('#previewLayer').jqm().jqDrag('#handle'); //set previewLayer as a jqModal window
				
				$('#previewButton').click(function()
				{
					var content = generatePreview();
					
					$('#previewContent').empty().append(content); //empty previewContent and add the generated preview
					$('#previewLayer').jqmShow(); //show previewLayer
				});
			});
		</script>
<script type="text/javascript">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-22592514-2']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

</script>
</head>
<body>
<center>
<div class="scroll">
<div class="registrationbtn" id="previewButton"><registrationbtn>registration</registrationbtn></a></div>

<?php
require_once 'online.php';

echo '<a href="alpha.zip" class="downloadbtn"><downloadbtn>download</downloadbtn></a>';
echo '<a href="/forum/" class="forumbtn"><forumbtn>forum</forumbtn></a>';

$res = get_online();


if ($res == 0) {
	echo '<div class="playersonlinelbl"><playersonlinelbl>0</playersonlinelbl></div>
	<div class="serverstatuslbl"><serverstatuslbl>Down</serverstatuslbl></div>';
} else {
	//if ((time() - $res['time']) > 30) $status = "Down"; else $status = "Ok"; 
	$status = 'OK';
	
	echo '<div class="playersonlinelbl"><playersonlinelbl>'.$res['count'].'</playersonlinelbl></div>
	<div class="serverstatuslbl"><serverstatuslbl>'.$status.'</serverstatuslbl></div>';
}

?>
</div>
</center>
		<div id="previewLayer" class="jqmWindow">
				<span style="display:block;text-align:right;font-size:25px;margin:0;padding:0;font-family:Arial;padding-right:15;"><a href="#" class="jqmClose"><b>X</b></a></span>
			<div id="previewContent">
			</div>
		</div>
</body>
</html>