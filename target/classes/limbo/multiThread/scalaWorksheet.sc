import java.util.concurrent.{Callable, ExecutorService, Executors}

import functions.AnomalousAnalysis.Actor
val es = Executors.newFixedThreadPool(4)

println("currentThread: " +  Thread.currentThread().getName)
val echoer = Actor[String](es){
  msg => println( "currentThread: " +  Thread.currentThread().getName + " " + s"Got message: '$msg'")
}
println(Thread.currentThread().getName  + " are you being blocked????")

echoer ! "hello"

echoer ! "Good bye"


