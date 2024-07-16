package openjoe.smart.sso.server.util;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import java.security.Key;
import java.util.Base64;

public class ClientCredentialsGenerator {

    private static final String AES = "AES";
    private static final String SALT = "`1qazx";

    public static String generateClientSecret(String clientId) {
        try {
            byte[] keyBytes = new byte[16];
            System.arraycopy(SALT.getBytes(), 0, keyBytes, 0, SALT.length());
            Key key = new SecretKeySpec(keyBytes, AES);
            Cipher cipher = Cipher.getInstance(AES);
            cipher.init(Cipher.ENCRYPT_MODE, key);
            byte[] encrypted = cipher.doFinal(clientId.getBytes());
            return Base64.getEncoder().encodeToString(encrypted);
        } catch (Exception e) {
            throw new RuntimeException("Error generating client secret", e);
        }
    }

    public static void main(String[] args) {
        String clientId = "1002";
        System.err.println(generateClientSecret(clientId));
    }
}
