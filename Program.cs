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
            // Check for service mode or console mode
            if ((Environment.UserInteractive) || OSUtils.IsUnix) {
                // Console mode, check for arguments
                if (args.Length > 0) {
                    try {
                        // Check entire parameter string for service install or uninstall request
                        string ParameterString = string.Concat(args).TrimStart('/').TrimStart('-');
                        switch (ParameterString) {
                            case "i":
                            case "install":
                                Console.WriteLine("Installing service...");
                                ManagedInstallerClass.InstallHelper(new string[] { Assembly.GetExecutingAssembly().Location });
                                Console.WriteLine("Service installed successfully!");
                                return;
                            case "u":
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

                    Console.WriteLine("Press Q to Quit...");

                    while (true) {
                        if (Console.KeyAvailable) {
                            if (Console.ReadKey(true).Key == ConsoleKey.Q) {
                                break;
                            } else {
                                Console.WriteLine(fTelnetProxy.ClientConnectionCount.ToString() + " active connections");
                                Console.WriteLine("Press Q to Quit...");
                            }
                        } else {
                            Thread.Sleep(100);
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
        }
    }
}
