package ca.uwaterloo.flix.language.phase.jvm;

import jdk.nashorn.internal.codegen.CompilerConstants;

import java.util.concurrent.Callable;

public class RunnableSpawn implements Runnable {
    private Callable fn;

    public RunnableSpawn(Callable func){
        fn = func;
    }

    public void run() {
        try{
            fn.call();
        } catch (java.lang.Exception ex) {

        }
    }
}

