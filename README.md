
Transclude text by references. Transcluded text is not saved in refernce text when saved.


## Usage

To transclude tax, run `M-x emacs-transclusion/transclude-data-for-current-buffer`

Example transclusion syntax
~~~
[EMBED: /tmp/somefile.txt]
[EMBED: :org-header-name "* header1" /tmp/ref.org]
~~~


## TODO
- Specify Org-Mode syntax to provide "org-modey way" of text transclusion, ex:

~~~
#+BEGIN_TRANSCLUDE_REF /some/file.txt
Some test
#+END_TRANSCLUDE_REF
~~~

- Transclusion of ORG text by referencs of ORG headers
- Create Melpa Package
- Allow users to extend code to use cusotm reference resolution functions
- Allow users to use custom transclusion markup
- Allow modification of transcluded text to modify origianl transcluded source
- Add read-only text attribute to transcluded text by default


