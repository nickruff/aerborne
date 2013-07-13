function updateDivText() {
	$("div#eq1 ").append("$b \\triangleq \\dfrac{1 ~~~~ 2}{3}$");
}

function processDiv() { 
	MathJax.Hub.Queue(["Typeset",MathJax.Hub,"eq1"]);	
}
