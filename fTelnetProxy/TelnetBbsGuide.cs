using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Serialization;

namespace RandM.fTelnetProxy {
    public class TelnetBbsGuide {
        public static bool IsInDialDirectory(string hostname, int port) {
            // Reject common service ports (list taken from grc.com)
            int[] commonServicePorts = {
                0, 21, 22, 25, 79, 80, 110, 113, 119, 135, 139, 143, 389, 443, 445, 1002, 1024, 1025, 1026, 1027, 1028, 1029, 1030, 1720, 5000
            };
            if (commonServicePorts.Contains(port)) {
                return false;
            }

            // Ensure dialdirectory.xml exists
            if (!File.Exists("dialdirectory.xml")) {
                return false;
            }

            // Read dialing directory and transform & to &amp; because a bare & is not valid XML and will fail to parse
            string xml = File.ReadAllText("dialdirectory.xml");
            xml = xml.Replace("&", "&amp;");

            // Deserialize the dialing directory and check whether the given host:port exist with protocol="TELNET"
            XmlSerializer serializer = new XmlSerializer(typeof(EtherTerm));
            using (StringReader reader = new StringReader(xml)) {
                EtherTerm dialDirectory = (EtherTerm)serializer.Deserialize(reader);
                return dialDirectory.Phonebook.BBS.Any(x => x.Ip.Equals(hostname, System.StringComparison.OrdinalIgnoreCase) && (x.Port == port) && x.Protocol.Equals("telnet", System.StringComparison.OrdinalIgnoreCase));
            }
        }

        [XmlRoot(ElementName = "BBS")]
        public class BBS {

            [XmlAttribute(AttributeName = "name")]
            public string Name { get; set; }

            [XmlAttribute(AttributeName = "ip")]
            public string Ip { get; set; }

            [XmlAttribute(AttributeName = "port")]
            public int Port { get; set; }

            [XmlAttribute(AttributeName = "protocol")]
            public string Protocol { get; set; }

            [XmlAttribute(AttributeName = "login")]
            public string Login { get; set; }

            [XmlAttribute(AttributeName = "password")]
            public string Password { get; set; }

            [XmlAttribute(AttributeName = "font")]
            public string Font { get; set; }

            [XmlAttribute(AttributeName = "keyMap")]
            public string KeyMap { get; set; }
        }

        [XmlRoot(ElementName = "Phonebook")]
        public class Phonebook {

            [XmlElement(ElementName = "BBS")]
            public List<BBS> BBS { get; set; }

            [XmlAttribute(AttributeName = "version")]
            public double Version { get; set; }
        }

        [XmlRoot(ElementName = "EtherTerm")]
        public class EtherTerm {

            [XmlElement(ElementName = "Phonebook")]
            public Phonebook Phonebook { get; set; }
        }
    }
}
