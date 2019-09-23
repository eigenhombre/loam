loam: *.lisp *.asd
	buildapp --output loam                 \
		 --asdf-path .                 \
		 --asdf-tree ~/quicklisp/dists \
		 --load-system loam            \
		 --entry loam:main

clean:
	rm loam *.fasl README.html
