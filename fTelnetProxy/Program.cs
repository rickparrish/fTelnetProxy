using RandM.RMLib;
using System;
using System.Configuration.Install;
using System.Reflection;
using System.ServiceProcess;
using System.Threading;

namespace RandM.fTelnetProxy {
    static class Program {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        static void Main(string[] args) {
            try {
                // Level filtering will happen in the event handler
                // This is because we want to log everything to disk, and only filter to the console
                RMLog.Level = LogLevel.Trace;

                // Check for service mode or console mode
                if (Environment.UserInteractive || OSUtils.IsUnix) {
                    // Console mode, check for arguments
                    if (args.Length > 0) {
                        try {
                            // Check entire parameter string for service install or uninstall request
                            string ParameterString = string.Concat(args).TrimStart('/').TrimStart('-');
                            switch (ParameterString) {
                                case "install":
                                    Console.WriteLine("Installing service...");
                                    ManagedInstallerClass.InstallHelper(new string[] { Assembly.GetExecutingAssembly().Location });
                                    Console.WriteLine("Service installed successfully!");
                                    return;
                                case "uninstall":
                                    Console.WriteLine("Uninstalling service...");
                                    ManagedInstallerClass.InstallHelper(new string[] { "/u", Assembly.GetExecutingAssembly().Location });
                                    Console.WriteLine("Service uninstalled successfully!");
                                    return;
                            }
                        } catch (Exception ex) {
                            Console.WriteLine("Error handling service request: " + ex.Message);
                            return;
                        }
                    }

                    // If we get here, we're running as console app
                    using (var fTelnetProxy = new fTelnetProxy()) {
                        fTelnetProxy.Start();

                        bool doRecycle = false;
                        string keyMessage = "Press A for Active Connections, R to Recycle, or Q to Quit...";
                        RMLog.Info(keyMessage);

                        while (true) {
                            if (Console.KeyAvailable) {
                                var Ch = Console.ReadKey(true).Key;
                                if (Ch == ConsoleKey.A) {
                                    fTelnetProxy.DisplayActiveConnections();
                                    if (doRecycle) {
                                        RMLog.Info("- Will recycle when all clients disconnect");
                                    }
                                } else if (Ch == ConsoleKey.Q) {
                                    break;
                                } else if (Ch == ConsoleKey.R) {
                                    if (fTelnetProxy.ClientConnectionCount == 0) {
                                        // No connections, so quit
                                        break;
                                    } else {
                                        // Have connections, so toggle recycle flag
                                        doRecycle = !doRecycle;
                                        RMLog.Info($"Recycle flag: {(doRecycle ? "set" : "unset")}");
                                    }
                                } else {
                                    RMLog.Info(keyMessage);
                                }
                            } else {
                                Thread.Sleep(1000);

                                // Asked to recycle, and no connections, so quit
                                if (doRecycle && (fTelnetProxy.ClientConnectionCount == 0)) {
                                    break;
                                }
                            }
                        }

                        Console.WriteLine("Exiting...");

                        fTelnetProxy.Stop();
                    }
                } else {
                    // Service mode
                    using (var fTelnetProxyService = new Service()) {
                        ServiceBase.Run(fTelnetProxyService);
                    }
                }
            } catch (Exception ex) {
                RMLog.Exception(ex, "Unhandled exception in main program loop");
            }
        }
    }
}
