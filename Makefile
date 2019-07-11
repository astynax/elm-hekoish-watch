elm.min.js: elm.js
	closure-compiler elm.js > elm.min.js

elm.js: Main.elm
	elm make Main.elm --optimize --output=elm.js
