using RandM.RMLib;
using System;
using System.Collections.Generic;
using System.Text;

namespace RandM.fTelnetProxy
{
    public class Config : ConfigHelper
    {
        public string CertFilename { get; set; }
        public string CertPassword { get; set; }
        public string RelayFilename { get; set; }
        public string TargetHostname { get; set; }
        public int TargetPort { get; set; }
        public int WsPort { get; set; }
        public int WssPort { get; set; }

        static public Config Default = new Config();

        public Config(): base()
        {
            CertFilename = "";
            CertPassword = "";
            RelayFilename = "";
            TargetHostname = "localhost";
            TargetPort = 23;
            WsPort = 0;
            WssPort = 0;

            base.Load();
        }
    }
}
