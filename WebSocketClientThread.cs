using System.Threading;
using System;
using System.IO;
using System.Reflection;
using System.Text;

namespace RandM.RMLib
{
    public class WebSocketClientThread : RMThread
    {
        public event EventHandler CloseEvent = null;
        public event EventHandler<StringEventArgs> ErrorMessageEvent = null;
        public event EventHandler<StringEventArgs> MessageEvent = null;

        private WebSocketConnection _InConnection = null;
        private TcpConnection _OutConnection = null;

        public WebSocketClientThread(WebSocketConnection connection)
        {
            _InConnection = connection;
        }

        protected override void Execute()
        {
            // Read the Host:Port line from fTelnet (should be bbs.ftelnet.ca:23\r\n for example)
            string HostAndPort = _InConnection.ReadLn(5000);
            if (_InConnection.ReadTimedOut)
            {
                RaiseErrorMessageEvent("Timeout waiting for Host:Port");
                _InConnection.Close();
                return;
            }

            // Split the Host:Port into their respective pieces
            string Host = "";
            int Port = 23;
            if (HostAndPort.Contains(":"))
            {
                Host = HostAndPort.Split(':')[0];
                int.TryParse(HostAndPort.Split(':')[1], out Port);
            }
            else
            {
                Host = HostAndPort;
            }

            // Ensure the requested port is 23
            // TODO Add an option to allow or deny non-23 destination
            if (Port != 23)
            {
                RaiseErrorMessageEvent("Refused non-port-23 destination request: " + HostAndPort);
                _InConnection.Close();
                return;
            }

            // Try to connect to the desired Host and Port
            _OutConnection = new TcpConnection();
            if (_OutConnection.Connect(Host, Port))
            {
                RaiseMessageEvent("Connected to " + HostAndPort);

                bool DoSleep = true;

                while (!_Stop && _InConnection.Connected && _OutConnection.Connected)
                {
                    DoSleep = true;

                    if (_InConnection.CanRead())
                    {
                        DoSleep = false;
                        _OutConnection.WriteBytes(_InConnection.ReadBytes());
                    }

                    if (_OutConnection.CanRead())
                    {
                        DoSleep = false;
                        byte[] Bytes = _OutConnection.ReadBytes();
                        string Text = Encoding.Default.GetString(Bytes);
                        File.AppendAllText(StringUtils.PathCombine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "fTelnetProxy_Spy.log"), Text, Encoding.Default); // TODOX
                        _InConnection.WriteBytes(Bytes);
                    }

                    if (DoSleep) Thread.Sleep(1);
                }

                _OutConnection.Close();
                _InConnection.Close();

                RaiseCloseEvent();
            }
            else
            {
                RaiseErrorMessageEvent("Unable to connect to " + HostAndPort);
            }
        }

        private void RaiseCloseEvent()
        {
            EventHandler Handler = CloseEvent;
            if (Handler != null) Handler(this, EventArgs.Empty);
        }

        private void RaiseErrorMessageEvent(string AMessage)
        {
            EventHandler<StringEventArgs> Handler = ErrorMessageEvent;
            if (Handler != null) Handler(this, new StringEventArgs("[" + _InConnection.GetRemoteIP() + ":" + _InConnection.GetRemotePort().ToString() + "] " + AMessage));
        }

        private void RaiseMessageEvent(string AMessage)
        {
            EventHandler<StringEventArgs> Handler = MessageEvent;
            if (Handler != null) Handler(this, new StringEventArgs("[" + _InConnection.GetRemoteIP() + ":" + _InConnection.GetRemotePort().ToString() + "] " + AMessage));
        }

        public override void Stop()
        {
            // Close the socket so that any waits on ReadLn(), ReadChar(), etc, will not block
            if (_OutConnection != null) _OutConnection.Close();
            if (_InConnection != null) _InConnection.Close();

            base.Stop();
        }
    }
}
