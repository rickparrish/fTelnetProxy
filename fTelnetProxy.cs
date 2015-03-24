using RandM.RMLib;
using System;
using System.IO;
using System.Reflection;

namespace RandM.fTelnetProxy
{
    public class fTelnetProxy : IDisposable
    {
        private string _ErrorLogFile = StringUtils.PathCombine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "fTelnetProxy_Error.log");
        private string _LogFile = StringUtils.PathCombine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "fTelnetProxy.log");
        
        FlashSocketPolicyServerThread _FSPS = null;
        ProxyServerThread _TelnetProxy = null;
        ProxyServerThread _WebSocketProxy = null;

        public fTelnetProxy()
        {
            MessageEvent(null, new StringEventArgs("fTelnetProxy Starting Up"));
            MessageEvent(null, new StringEventArgs("Starting Flash Socket Policy Thread"));
            try
            {
                _FSPS = new FlashSocketPolicyServerThread("0.0.0.0", 843, "2323, 11235"); // TODO 0.0.0.0, 843, 2323
                _FSPS.ErrorMessageEvent += new EventHandler<StringEventArgs>(ErrorMessageEvent);
                _FSPS.MessageEvent += new EventHandler<StringEventArgs>(MessageEvent);
                _FSPS.Start();
            }
            catch (Exception ex)
            {
                ErrorMessageEvent(null, new StringEventArgs("Failed to start Flash Socket Policy Server Thread: " + ex.Message));
                _FSPS = null;
            }

            MessageEvent(null, new StringEventArgs("Starting Telnet Proxy Thread"));
            try
            {
                _TelnetProxy = new ProxyServerThread("0.0.0.0", 2323, ConnectionType.None, ConnectionType.None); // TODO 0.0.0.0, 2323
                _TelnetProxy.ErrorMessageEvent += new EventHandler<StringEventArgs>(ErrorMessageEvent);
                _TelnetProxy.MessageEvent += new EventHandler<StringEventArgs>(MessageEvent);
                _TelnetProxy.Start();
            }
            catch (Exception ex)
            {
                ErrorMessageEvent(null, new StringEventArgs("Failed to start Telnet Proxy Thread: " + ex.Message));
                _TelnetProxy = null;
            }

            MessageEvent(null, new StringEventArgs("Starting WebSocket Proxy Thread"));
            try
            {
                _WebSocketProxy = new ProxyServerThread("0.0.0.0", 11235, ConnectionType.WebSocket, ConnectionType.Telnet); // TODO 0.0.0.0, 11235
                _WebSocketProxy.ErrorMessageEvent += new EventHandler<StringEventArgs>(ErrorMessageEvent);
                _WebSocketProxy.MessageEvent += new EventHandler<StringEventArgs>(MessageEvent);
                _WebSocketProxy.Start();
            }
            catch (Exception ex)
            {
                ErrorMessageEvent(null, new StringEventArgs("Failed to start WebSocket Proxy Thread: " + ex.Message));
                _WebSocketProxy = null;
            }
        }

        public void Dispose()
        {
            MessageEvent(null, new StringEventArgs("fTelnetProxy Shutting Down"));

            if (_WebSocketProxy != null)
            {
                MessageEvent(null, new StringEventArgs("Stopping WebSocket Proxy Thread"));
                _WebSocketProxy.Stop();
            }
            if (_TelnetProxy != null)
            {
                MessageEvent(null, new StringEventArgs("Stopping Telnet Proxy Thread"));
                _TelnetProxy.Stop();
            }
            if (_FSPS != null)
            {
                MessageEvent(null, new StringEventArgs("Starting Flash Socket Policy Thread"));
                _FSPS.Stop();
            }

            MessageEvent(null, new StringEventArgs("fTelnetProxy Terminated" + Environment.NewLine + Environment.NewLine));
        }

        private void ErrorMessageEvent(object sender, StringEventArgs mea)
        {
            string LogLine = "[" + DateTime.Now.ToString() + "] " + mea.Text + Environment.NewLine;
            FileUtils.FileAppendAllText(_ErrorLogFile, LogLine);
            if (Environment.UserInteractive) Console.Write(LogLine);
        }

        private void MessageEvent(object sender, StringEventArgs mea)
        {
            string LogLine = "[" + DateTime.Now.ToString() + "] " + mea.Text + Environment.NewLine;
            FileUtils.FileAppendAllText(_LogFile, LogLine);
            if (Environment.UserInteractive) Console.Write(LogLine);
        }
    }
}
