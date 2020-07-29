val t = osc.UDP.Transmitter("192.168.0.77" -> 57120)
t.connect()
// t ! osc.Message("/foo")
t ! osc.Message("/stop")
t ! osc.Message("/start")
t ! osc.Message("/set-volume", 0.5f)
t ! osc.Message("/set-volume", 0.0f)
t ! osc.Message("/set-volume", 1.0f)

val t1 = osc.UDP.Transmitter("192.168.0.43" -> 57120)
t1.connect()
t1 ! osc.Message("/set-volume", 0.1f)
t1 ! osc.Message("/start")
t1 ! osc.Message("/stop")

val t2 = osc.UDP.Transmitter("192.168.0.42" -> 57120)
t2.connect()
t2 ! osc.Message("/set-volume", 0.1f)
t2 ! osc.Message("/start")
t2 ! osc.Message("/stop")

val t3 = osc.UDP.Transmitter("192.168.0.40" -> 57120)
t3.connect()
t3 ! osc.Message("/set-volume", 0.1f)
t3 ! osc.Message("/start")
t3 ! osc.Message("/stop")

val t4 = osc.UDP.Transmitter("192.168.0.44" -> 57120)
t4.connect()
t4 ! osc.Message("/set-volume", 0.1f)
t4 ! osc.Message("/start")
t4 ! osc.Message("/stop")

val tAll = Seq(t1, t2, t3, t4)

// def sendAll(p: osc.Packet): Unit = tAll.foreach(_ ! p)

sendAll(osc.Message("/stop"))
sendAll(osc.Message("/start"))

sendAll(osc.Message("/set-volume", 1.0f))
sendAll(osc.Message("/set-volume", 0f))

// 1: 43, 2: 42, 3: 40, 4: 44


sendAll(osc.Message("/shutdown"))

val tAll = Seq(36, 37, 40, 42, 43, 44).map { dot =>
  val t = osc.UDP.Transmitter(s"192.168.0.$dot" -> 57120)
  t.connect()
  t
}

def sendAll(p: osc.Packet): Unit = tAll.foreach(_ ! p)

sendAll(osc.Message("/set-volume", 0.0f))
sendAll(osc.Message("/set-volume", 0.25f))
sendAll(osc.Message("/set-volume", 0.5f))

sendAll(osc.Message("/stop"))

tAll(0) ! osc.Message("/start")
tAll(1) ! osc.Message("/start")
tAll(2) ! osc.Message("/start")
tAll(3) ! osc.Message("/start")
tAll(4) ! osc.Message("/start")
tAll(5) ! osc.Message("/start")

tAll(0) ! osc.Message("/stop")
tAll(1) ! osc.Message("/stop")
tAll(2) ! osc.Message("/stop")
tAll(3) ! osc.Message("/stop")
tAll(4) ! osc.Message("/stop")
tAll(5) ! osc.Message("/stop")

sendAll(osc.Message("/shutdown"))
