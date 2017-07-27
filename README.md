### What

This is my own indentation engine written to indent code in [kotlin
mode](https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode) (hence
the names of some functions).

It's not ideal, but indents code in a way I want to see it (more
lisp-like). Unlike the original kotlin-mode's indentataion engine, this
handles multiline lists cases, like

```kotlin
class SomeClass<Generic,
                L> (constructorParam : Int,
                    otherParam : SomeOther<L,
                                           List<Maybe<L,
                                                      Int>>>,
                    thirdParam : String) {
    
    fun method(multi: Int,
               arg : String,
               list : Pew) {

        list
            .multilineCall(with,
                           more,
                           args,
                           () -> {
                               // and even such constructions!!

                               which
                                   .actually(can,
                                             be,
                                             inserted(one,
                                                      into,
                                                      one))
                           })

        synchronized(this)  
        {
            // and blocks of this type, of course            
        } 
        
    }
}
```

basically, rules are: every "{" block indents +4 (can be tuned), any "("
and "<" blocks serve as indentation base, i.e. all next lines will be
indented relative to it.

Can be used like this:

```emacs-lisp
(defun kotlin-mode-use-my-kotlin-indent ()
  (setq indent-line-function 'kotlin-mode--elk-indent-line))

(add-hook 'kotlin-mode-hook 'kotlin-mode-use-my-kotlin-indent)
```

this will override original kotlin's mode indentation.
