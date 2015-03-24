using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.ServiceProcess;
using System.Text;

namespace RandM.fTelnetProxy
{
    public partial class Service : ServiceBase
    {
        private fTelnetProxy _fTelnetProxy;

        public Service()
        {
            InitializeComponent();
            _fTelnetProxy = new fTelnetProxy();
        }

        protected override void OnStart(string[] args)
        {
            _fTelnetProxy.Start();
        }

        protected override void OnStop()
        {
            _fTelnetProxy.Stop();
        }
    }
}
