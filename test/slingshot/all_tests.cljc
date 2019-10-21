(ns slingshot.all-tests
  (:require
    #?(:cljs [cljs.test :refer-macros [run-tests]])
    #?(:cljs [doo.runner :refer-macros [doo-tests]])
             [slingshot.slingshot-test]
             [slingshot.support-test]
             [slingshot.test-test]))

#?(:cljs (enable-console-print!))

#?(:cljs (doo-tests 'slingshot.slingshot-test ; For Doo
                    'slingshot.support-test
                    'slingshot.test-test))

#?(:cljs (run-tests 'slingshot.slingshot-test ; For Figwheel
                    'slingshot.support-test
                    'slingshot.test-test))
