package main

import "github.com/proxypoke/i3ipc"

func main() {
	println("Hello, world.")
	ipcsocket, _ := i3ipc.GetIPCSocket()
	version, _ := ipcsocket.GetVersion()
	println("Version:", version.Human_Readable)
}
