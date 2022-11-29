using RandM.RMLib;
using System;
using System.Collections.Generic;
using System.IO;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace RandM.fTelnetProxy {
    public class WebSocketServerThread : RMThread {
        public readonly int Port;

        private string _Address;
        private int _ClientThreadCounter = 0;
        private List<WebSocketClientThread> _ClientThreads = new List<WebSocketClientThread>();
        private object _ClientThreadsLock = new object();
        private WebSocketConnection _Server;

        public event EventHandler ClientCountEvent = null;
        public event EventHandler ListeningEvent = null;

        public WebSocketServerThread(string address, int port) {
            _Address = address;
            Port = port;
        }

        public int ClientConnectionCount {
            get {
                lock (_ClientThreadsLock) {
                    return _ClientThreads.Count;
                }
            }
        }

        void ClientThread_FinishEvent(object sender, EventArgs e) {
            if (sender is WebSocketClientThread) {
                lock (_ClientThreadsLock) {
                    if (_ClientThreads.Contains((WebSocketClientThread)sender)) {
                        try {
                            ((WebSocketClientThread)sender).Dispose();
                        } catch (Exception ex) {
                            RMLog.Exception(ex, "Exception while disposing client thread while handling client thread finish event");
                        } finally {
                            _ClientThreads.Remove((WebSocketClientThread)sender);
                        }
                    } else {
                        RMLog.Error("ClientThread_FinishEvent did not find sender in _ClientThreads (sender=" + sender.ToString() + ")");
                    }
                }

                RaiseClientCountEvent();
            } else {
                RMLog.Error("ClientThread_FinishEvent's sender is not a WebSocketClientThread (sender=" + sender.ToString() + ")");
            }
        }

        public void DisplayActiveConnections() {
            lock (_ClientThreadsLock) {
                foreach (var ClientThread in _ClientThreads) {
                    if (ClientThread != null) {
                        try {
                            ClientThread.DisplayConnectionInformation();
                        } catch (Exception ex) {
                            RMLog.Exception(ex, "Error listing client thread details");
                        }
                    }
                }
            }
        }

        protected override void Execute() {
            using (_Server = new WebSocketConnection()) {
                while (!_Stop) {
                    if (_Server.Listen(_Address, Port)) {
                        RMLog.Info($"WebSocket Server Thread listening on {_Address}:{Port}");
                        RaiseListeningEvent();

                        while (!_Stop) {
                            try {
                                // Accept an incoming connection
                                if (_Server.CanAccept(1000)) // 1 second
                                {
                                    Socket NewSocket = _Server.Accept();
                                    if (NewSocket != null) {
                                        lock (_ClientThreadsLock) {
                                            WebSocketClientThread ClientThread = new WebSocketClientThread(NewSocket, ++_ClientThreadCounter, Port);
                                            ClientThread.FinishEvent += ClientThread_FinishEvent;
                                            _ClientThreads.Add(ClientThread);
                                            ClientThread.Start();
                                        }

                                        RaiseClientCountEvent();
                                    }
                                }
                            } catch (Exception ex) {
                                RMLog.Exception(ex, "Unable to accept new websocket connection");
                            }
                        }

                        // Stop client threads
                        int ClientThreadCount = 0;
                        lock (_ClientThreadsLock) {
                            foreach (var ClientThread in _ClientThreads) {
                                if (ClientThread != null) {
                                    ClientThread.Stop();
                                }
                            }
                            ClientThreadCount = _ClientThreads.Count;
                        }

                        // Wait for client threads
                        while (ClientThreadCount > 0) {
                            lock (_ClientThreadsLock) {
                                ClientThreadCount = _ClientThreads.Count;
                            }
                            Thread.Sleep(100);
                        }
                    } else {
                        RMLog.Error($"WebSocket Server Thread: Unable to listen on {_Address}:{Port}, retrying in 15 seconds");
                        for (int i = 0; i < 15; i++) {
                            Thread.Sleep(1000);
                            if (_Stop) {
                                break;
                            }
                        }
                    }
                }
            }
        }

        private void RaiseClientCountEvent() {
            ClientCountEvent?.Invoke(this, EventArgs.Empty);
        }

        private void RaiseListeningEvent()
        {
            ListeningEvent?.Invoke(this, EventArgs.Empty);
        }

        public override void Stop() {
            // Close the socket so that any waits on ReadLn(), ReadChar(), etc, will not block
            if (_Server != null) {
                _Server.Close();
            }

            base.Stop();
        }
    }
}
