<!DOCTYPE html PUBLIC
          "-//W3C//DTD XHTML 1.0 Transitional//EN"
          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>Find the longest palindrome!</title>
<style type="text/css">
<!--
body,td,th {
  font-family: Verdana, Arial, Helvetica, sans-serif;
  font-size: x-large;
  color: #CCCCCC;
}

body {
  background-color: #333399;
}

.question { color: #FFCC33; }
.emph     { color: #99ee99; }
.alert    { color: #ee77aa; }

.right {
  color: #33FF66;
  font-weight: bold;
}
.wrong {
  color: #FF3366;
  font-weight: bold;
}

a:link    { color: #CCFFFF; }
a:visited { color: #CCFFFF; }

input {
  background-color: #eeee66;
  color: #333399;
}

code {
  font-family: Consolas, "Andale Mono", "Courier New", monospace, sans-serif;
  color: #eeee99;
  font-size: 120%;
}

span.removed {
  color: #ff9977;
  text-decoration: underline red;
}

code.block {
  background-color: #66eeee;
  color: #993333;
  overflow-wrap: break-word;
  display: block;
  border: 1px solid black;
  padding: 8px;
  width: 95%;
  line-height: 1em;
  margin-top: 0.25em;
  margin-bottom: 0.25em;
}

input.box {
  overflow-wrap: break-word;
  font-family: Consolas, "Andale Mono", "Courier New", monospace, sans-serif;
  font-size: 120%;
  color: #333333;
  border: 1px solid black;
  padding: 8px;
}

input.button {
  font-size: 120%;
  background-color: #99ee99;
  color: #333399;
  border: 1px solid black;
  padding: 8px;
}

-->
</style>
</head>
<body>
<h1>Find the longest palindrome!</h1>

<p>I'll give you a string of (up to 1000) letters
  and I need you to do one simple thing:
</p>
<p>Find the <span class="emph">least</span> possible number of letters that,
  if removed from the given string, what remains is a
  <span class="emph">palindrome</span>.
</p>
<blockquote>
  <p>For example, given the string:
    <code>bbccaddabaddacaaacdb</code>
    the correct answer is <span class="emph">5</span>.
  </p>
  <p>If one removes these five underlined letters:
    <code>b<span class="removed">b</span>ccaddabaddac<span class="removed">aaa</span>c<span class="removed">d</span>b</code>
    then the remaining string:
    <code>bccaddabaddaccb</code>
    is indeed a palindrome.  It is not possible to obtain a
    palindrome by removing fewer than five letters.
  </p>
</blockquote>

<hr />

<p><span class="question">Question 1</span>:
length 5  <code class="block" id="question">ababa</code>
</p>

<form action="/pl2/2018b/exercises/palseq.php" id="f" name="f" method="post">
What is the least number of characters you need to remove?
<table border="0" cellspacing="3">
<tr>
  <td><input type="text" class="box" name="answer" id="answer" autofocus /></td>
  <td><input type="submit" class="button" name="submit" id="submit" value="Submit!" /></td>
</tr>
</table>
</form>

</body>
</html>
