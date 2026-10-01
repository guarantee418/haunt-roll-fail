package hrf.gg

import java.io.InputStream
import java.security.{ KeyStore, SecureRandom, Principal, PrivateKey }
import java.security.cert.X509Certificate
import java.net.Socket

import java.io.File
import javax.net.ssl.{ KeyManagerFactory, SSLContext, SSLEngine, X509ExtendedKeyManager }
import akka.http.scaladsl.{ ConnectionContext, Http, HttpsConnectionContext }

object Ssl {
    def loadKeyManager(pkcs12File : File, pkcs12Password : String) : X509ExtendedKeyManager = {
        util.Using(new java.io.FileInputStream(pkcs12File)) { contentStream =>
            val ks = KeyStore.getInstance("PKCS12")
            ks.load(contentStream, pkcs12Password.toCharArray)
            ks
        }.map {
            keyStore =>
            val keyManagerFactory = KeyManagerFactory.getInstance("SunX509")
            keyManagerFactory.init(keyStore, pkcs12Password.toCharArray)
            keyManagerFactory.getKeyManagers.collectFirst { case m : X509ExtendedKeyManager => m }.get
        }.get
    }

    // Re-reads the certificate file when it changes, so a renewed certificate is used without a restart
    class ReloadingKeyManager(pkcs12File : File, pkcs12Password : String) extends X509ExtendedKeyManager {
        @volatile private var modified = pkcs12File.lastModified
        @volatile private var delegate = loadKeyManager(pkcs12File, pkcs12Password)
        @volatile private var checked = System.currentTimeMillis

        private def current : X509ExtendedKeyManager = {
            val now = System.currentTimeMillis

            if (now - checked > 60 * 1000) {
                checked = now

                val m = pkcs12File.lastModified

                if (m != modified) {
                    modified = m

                    try {
                        delegate = loadKeyManager(pkcs12File, pkcs12Password)
                        println("Reloaded certificate.")
                    }
                    catch {
                        case e : Exception => println("Failed to reload certificate: " + e)
                    }
                }
            }

            delegate
        }

        def getClientAliases(keyType : String, issuers : Array[Principal]) = current.getClientAliases(keyType, issuers)
        def chooseClientAlias(keyType : Array[String], issuers : Array[Principal], socket : Socket) = current.chooseClientAlias(keyType, issuers, socket)
        def getServerAliases(keyType : String, issuers : Array[Principal]) = current.getServerAliases(keyType, issuers)
        def chooseServerAlias(keyType : String, issuers : Array[Principal], socket : Socket) = current.chooseServerAlias(keyType, issuers, socket)
        def getCertificateChain(alias : String) = current.getCertificateChain(alias)
        def getPrivateKey(alias : String) = current.getPrivateKey(alias)
        override def chooseEngineClientAlias(keyType : Array[String], issuers : Array[Principal], engine : SSLEngine) = current.chooseEngineClientAlias(keyType, issuers, engine)
        override def chooseEngineServerAlias(keyType : String, issuers : Array[Principal], engine : SSLEngine) = current.chooseEngineServerAlias(keyType, issuers, engine)
    }

    def serverHttpsContext(pkcs12File : File, pkcs12Password : String) : HttpsConnectionContext = {
        val context = SSLContext.getInstance("TLS")
        context.init(Array(new ReloadingKeyManager(pkcs12File, pkcs12Password)), null, new SecureRandom)
        ConnectionContext.httpsServer(context)
    }
}
