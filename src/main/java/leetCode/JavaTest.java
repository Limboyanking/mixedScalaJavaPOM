package leetCode;

import java.util.Arrays;

public class JavaTest {

    public volatile int inc = 0;

    public void increase() {
        inc++;
    }

    public static void main(String[] args) {
//        final JavaTest test = new JavaTest();
//        for(int i=0;i<10;i++){
//            new Thread(){
//                public void run() {
//                    for(int j=0;j<1000;j++)
//                        test.increase();
//                };
//            }.start();
//        }
//
//        while(Thread.activeCount()>1)  //保证前面的线程都执行完
//            Thread.yield();
//        System.out.println(test.inc);



       String s = "";
       System.out.println(s + 'a');







    }



}

