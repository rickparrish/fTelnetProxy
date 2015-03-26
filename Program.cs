// TODO Check for code that needs to be rewritten
using System;
using System.Collections.Generic;
using System.Configuration.Install;
using System.Reflection;
using System.ServiceProcess;
using System.Text;

namespace RandM.fTelnetProxy
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        static void Main(string[] args)
        {
            if (Environment.UserInteractive)
            {
                if (args.Length > 0)
                {
                    try
                    {
                        string parameter = string.Concat(args);
                        switch (parameter)
                        {
                            case "/i":
                            case "-i":
                            case "/install":
                            case "--install":
                                Console.WriteLine("Installing service...");
                                ManagedInstallerClass.InstallHelper(new string[] { Assembly.GetExecutingAssembly().Location });
                                Console.WriteLine("Service installed successfully!");
                                return;
                            case "/u":
                            case "-u":
                            case "/uninstall":
                            case "--uninstall":
                                Console.WriteLine("Uninstalling service...");
                                ManagedInstallerClass.InstallHelper(new string[] { "/u", Assembly.GetExecutingAssembly().Location });
                                Console.WriteLine("Service uninstalled successfully!");
                                return;
                        }
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine("Error handling service request: " + ex.Message);
                        return;
                    }
                }

                // If we get here, we're running as console app
                using (var fTelnetProxyApp = new fTelnetProxy())
                {
                    do
                    {
                        Console.WriteLine("Press Q to Quit...");
                    } while (Console.ReadKey(true).Key != ConsoleKey.Q);
                }
            }
            else
            {
                // running as service
                using (var fTelnetProxyService = new Service())
                {
                    ServiceBase.Run(fTelnetProxyService);
                }
            }
        }
    }
}
