using System;
using System.Collections.Generic;
using System.ServiceProcess;
using System.Text;

namespace RandM.fTelnetProxy
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        static void Main()
        {
            if (!Environment.UserInteractive)
                // running as service
                using (var fTelnetProxyService = new Service())
                {
                    ServiceBase.Run(fTelnetProxyService);
                }
            else
            {
                // running as console app
                using (var fTelnetProxyApp = new fTelnetProxy())
                {
                    do
                    {
                        Console.WriteLine("Press Q to Quit...");
                    } while (Console.ReadKey(true).Key != ConsoleKey.Q);
                }
            }
        }
    }
}
