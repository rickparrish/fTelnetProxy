using RandM.RMLib;
using System;
using System.IO;
using System.Reflection;
using System.Text;

namespace RandM.fTelnetProxy
{
    public class fTelnetProxy : IDisposable
    {
        private FileStream _LogStream = null;
        private object _LogStreamLock = new object();

        WebSocketServerThread _WebSocketServer = null;

        public fTelnetProxy()
        {
            _LogStream = new FileStream(Path.Combine(ProcessUtils.StartupPath, "fTelnetProxy.log"), FileMode.Append, FileAccess.Write, FileShare.Read);

            if (Config.Default.Loaded | ParseCommandLineArgs())
            {
                MessageEvent(null, new StringEventArgs("fTelnetProxy Starting Up"));
                MessageEvent(null, new StringEventArgs("Starting WebSocket Proxy Thread"));
                try
                {
                    _WebSocketServer = new WebSocketServerThread("0.0.0.0", Config.Default.ListenPort); // TODO wss://
                    _WebSocketServer.ErrorMessageEvent += ErrorMessageEvent;
                    _WebSocketServer.MessageEvent += MessageEvent;
                    _WebSocketServer.Start();
                }
                catch (Exception ex)
                {
                    ErrorMessageEvent(null, new StringEventArgs("Failed to start WebSocket Proxy Thread: " + ex.Message));
                    _WebSocketServer = null;
                }
            }
            else
            {
                ShowHelp();
            }
        }

        public void Dispose()
        {
            MessageEvent(null, new StringEventArgs("fTelnetProxy Shutting Down"));

            if (_WebSocketServer != null)
            {
                MessageEvent(null, new StringEventArgs("Stopping WebSocket Proxy Thread"));
                _WebSocketServer.Stop();
            }

            MessageEvent(null, new StringEventArgs("fTelnetProxy Terminated" + Environment.NewLine + Environment.NewLine));

            if (_LogStream != null)
            {
                _LogStream.Write(Encoding.ASCII.GetBytes(Environment.NewLine), 0, Environment.NewLine.Length);
                _LogStream.Close();
                _LogStream.Dispose();
            }
        }

        private void ErrorMessageEvent(object sender, StringEventArgs mea)
        {
            MessageEvent(sender, new StringEventArgs("ERROR: " + mea.Text));
        }

        private bool LoadConfigFile()
        {
            // TODO
            return false;
        }

        private void MessageEvent(object sender, StringEventArgs mea)
        {
            lock (_LogStreamLock)
            {
                byte[] MessageBytes = Encoding.ASCII.GetBytes("[" + DateTime.Now.ToString() + "] " + mea.Text + Environment.NewLine);
                _LogStream.Write(MessageBytes, 0, MessageBytes.Length);
                _LogStream.Flush();

                if (Environment.UserInteractive) Console.Write(Encoding.ASCII.GetString(MessageBytes));
            }
        }

        private bool ParseCommandLineArgs()
        {
            string[] Args = Environment.GetCommandLineArgs();
            for (int i = 1; i < Args.Length; i++)
            {
                switch (Args[i])
                {
                    case "-c":
                    case "--cert":
                        i += 1;
                        Config.Default.CertFilename = Args[i];
                        break;

                    case "-?":
                    case "-h":
                    case "--help":
                        ShowHelp();
                        return false;

                    case "-p":
                    case "--port":
                        i += 1;
                        try
                        {
                            Config.Default.ListenPort = Convert.ToInt16(Args[i]);
                        }
                        catch (Exception)
                        {
                            Console.WriteLine();
                            Console.WriteLine("Invalid port '" + Args[i] + "'");
                            Console.WriteLine();
                            return false;
                        }
                        break;

                    case "-pw":
                    case "--password":
                        i += 1;
                        Config.Default.CertPassword = Args[i];
                        break;

                    case "-r":
                    case "--relay":
                        i += 1;
                        Config.Default.RelayFilename = Args[i];
                        break;

                    case "-t":
                    case "--target":
                        i += 1;
                        if (Args[i].Contains(":"))
                        {
                            Config.Default.TargetHostname = Args[i].Split(':')[0];
                            try
                            {
                                Config.Default.TargetPort = Convert.ToInt16(Args[i].Split(':')[1]);
                            }
                            catch (Exception)
                            {
                                Console.WriteLine();
                                Console.WriteLine("Invalid target port '" + Args[i].Split(':')[1] + "'");
                                Console.WriteLine();
                                return false;
                            }
                        }
                        else
                        {
                            Config.Default.TargetHostname = Args[i];
                        }
                        break;

                    default:
                        ShowHelp();
                        Console.WriteLine();
                        Console.WriteLine("Error:");
                        Console.WriteLine();
                        Console.WriteLine("  Unknown parameter '" + Args[i] + "'");
                        Console.WriteLine();
                        return false;
                }
            }

            return true;
        }

        private void ShowHelp()
        {
            if (Environment.UserInteractive)
            {
                Console.WriteLine();
                Console.WriteLine("Usage: FleckProxy.exe [options]");
                Console.WriteLine();
                Console.WriteLine("Options:");
                Console.WriteLine();
                Console.WriteLine("  -ws <port>                 Port to listen for ws:// connections on");
                Console.WriteLine("  --ws <port>                Not supplying will disable ws:// connections");
                Console.WriteLine();
                Console.WriteLine("  -t <host:port>             Telnet server to redirect to");
                Console.WriteLine("  --target <host:port>       Default is localhost:23");
                Console.WriteLine();
                Console.WriteLine("  -wss <port>                Port to listen for wss:// connections on");
                Console.WriteLine("  --wss <port>               Not supplying will disable wss:// connections");
                Console.WriteLine();
                Console.WriteLine("  -c <filename>              PKCS12 file containing private key and cert chain");
                Console.WriteLine("  --cert <filename>          Only needed if your site uses https://");
                Console.WriteLine();
                Console.WriteLine("  -pw <password>             Password to use to open the PKCS12 file");
                Console.WriteLine("  --password <password>      Only needed if PKCS12 file is password protected");
                Console.WriteLine();
                Console.WriteLine("  -l <level>                 Set log level (Debug, Info, Warn, Error)");
                Console.WriteLine("  --loglevel <level>         Default is Info");
                Console.WriteLine();
                Console.WriteLine("  -?, -h, --help             Display this screen");
                Console.WriteLine();
                Console.WriteLine("Notes:");
                Console.WriteLine();
                Console.WriteLine("- You must pass a port via either --ws (plain ws:// connection) or");
                Console.WriteLine("  --wss (ssl wss:// connection) to start the server.  You can also pass both");
                Console.WriteLine("  plain and ssl ports by using --ws and --wss at the same time");
                Console.WriteLine();
                Console.WriteLine("- If you pass a port via --wss, then you must also pass a PKCS12 certificate");
                Console.WriteLine("  via the --cert parameter.  If the certificate is password protected, then");
                Console.WriteLine("  you must also pass the --password parameter.  Note the password may be");
                Console.WriteLine("  visible to other users of the system, so you may be better off with a");
                Console.WriteLine("  password-less certificate that is secured so only you have access to read it");
                //Console.WriteLine("345678901234567890123456789012345678901234567890123456789012345678901234567890");
            }
        }
    }
}
