using System.Threading;
using System;
using System.IO;
using System.Reflection;
using System.Text;
using RandM.RMLib;

namespace RandM.fTelnetProxy
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
            // Defaults for redirect location
            string Hostname = Config.Default.TargetHostname;
            int Port = Config.Default.TargetPort;

            // Check if we should override the defaults with user selected values
            if (!string.IsNullOrEmpty(Config.Default.RelayFilename))
            {
                bool CanRelay = false;

                string[] HostAndPort = _InConnection.Header["Path"].Split('/');
                if ((HostAndPort.Length == 3) && (int.TryParse(HostAndPort[2], out Port)))
                {
                    Hostname = HostAndPort[1];

                    if (File.Exists(Config.Default.RelayFilename))
                    {
                        try
                        {
                            string[] AllowedHosts = File.ReadAllLines(Config.Default.RelayFilename);
                            if (AllowedHosts.Length > 0)
                            {
                                // Check for a whitelisted port
                                string[] AllowedPorts = AllowedHosts[0].Split(',');
                                foreach (string AllowedPort in AllowedPorts)
                                {
                                    if (AllowedPort == Port.ToString())
                                    {
                                        CanRelay = true;
                                        break;
                                    }
                                }

                                // Not a whitelisted port, check for a whitelisted host
                                if (!CanRelay)
                                {
                                    string RequestedHostPort = Hostname + ":" + Port.ToString();
                                    foreach (string AllowedHost in AllowedHosts)
                                    {
                                        if (AllowedHost == RequestedHostPort)
                                        {
                                            CanRelay = true;
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                        catch (Exception ex)
                        {
                            RaiseErrorMessageEvent("Error reading relay file '" + Config.Default.RelayFilename + "' (Exception=" + ex.Message + ")");
                        }
                    }
                    else
                    {
                        RaiseErrorMessageEvent("Relay file '" + Config.Default.RelayFilename + "' does not exist");
                    }
                }

                if (!CanRelay)
                {
                    RaiseMessageEvent("Rejecting request for " + Hostname + ":" + Port.ToString());
                    _InConnection.WriteLn("Sorry, for security reasons this proxy won't connect to " + Hostname + ":" + Port.ToString());
                    Thread.Sleep(2500);
                    _InConnection.Close();
                    return;
                }
            }

            // Try to connect to the desired Host and Port
            _InConnection.WriteLn("Connecting to " + Hostname + ":" + Port.ToString() + "...");
            _OutConnection = new TcpConnection();
            if (_OutConnection.Connect(Hostname, Port))
            {
                RaiseMessageEvent("Connected to " + Hostname + ":" + Port.ToString());

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
                RaiseErrorMessageEvent("Unable to connect to " + Hostname + ":" + Port.ToString());
                _InConnection.WriteLn("Sorry, I wasn't able to connect to " + Hostname + ":" + Port.ToString());
                Thread.Sleep(2500);
                _InConnection.Close();
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
