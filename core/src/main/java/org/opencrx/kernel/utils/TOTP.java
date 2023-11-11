/*
 * ====================================================================
 * Project:     openCRX/Core, http://www.opencrx.org/
 * Description: TOTP
 * Owner:       the original authors.
 * ====================================================================
 *
 * This software is published under the BSD license
 * as listed below.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in
 * the documentation and/or other materials provided with the
 * distribution.
 * 
 * * Neither the name of the openCRX team nor the names of the contributors
 * to openCRX may be used to endorse or promote products derived
 * from this software without specific prior written permission
 * 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 * ------------------
 * 
 * This product includes software developed by the Apache Software
 * Foundation (http://www.apache.org/).
 * 
 * This product includes software developed by contributors to
 * openMDX (http://www.openmdx.org/)
 */
package org.opencrx.kernel.utils;

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

import org.opencrx.application.uses.org.apache.commons.codec.binary.Base32;

/**
 * Time-based One-time Password (TOTP)
 * 
 */
public abstract class TOTP {
	
	/**
	 * Get time-based one-time password.
	 * 
	 * @param secretKey - secret credential key (HEX)
	 * @return the OTP
	 * @throws NoSuchAlgorithmException 
	 * @throws InvalidKeyException 
	 */
	public static String getPassword(
		String secretKey
	) throws InvalidKeyException, NoSuchAlgorithmException {
		return getPassword(getTime(), secretKey);
	}

	/**
	 * Validate one-time password.
	 * 
	 * @param secretKey - secret credential key (HEX)
	 * @param otp - OTP to validate
	 * @return valid?
	 * @throws NoSuchAlgorithmException 
	 * @throws InvalidKeyException 
	 */
	public static boolean validatePassword(
		final String secretKey,
		final String otp
	) throws InvalidKeyException, NoSuchAlgorithmException {
		return validatePassword(
			getTime(),
			secretKey,
			otp
		);
	}

	/**
	 * Validate one-time password.
	 * 
	 * @param time
	 * @param secretKey
	 * @param otp
	 * @return
	 * @throws NoSuchAlgorithmException 
	 * @throws InvalidKeyException 
	 */
	private static boolean validatePassword(
		final long time,
		final String secretKey,
		final String otp
	) throws InvalidKeyException, NoSuchAlgorithmException {
		return
			getPassword(time, secretKey).equals(otp) || 
			(time > 0 && getPassword(time - 1, secretKey).equals(otp));
	}

	/**
	 * Get OTP time.
	 * 
	 * @return
	 */
	private static long getTime(
	) {
		return System.currentTimeMillis() / 30000;
	}

	/**
	 * Get one-time password.
	 * 
	 * @param time
	 * @param secretKey
	 * @return
	 * @throws NoSuchAlgorithmException 
	 * @throws InvalidKeyException 
	 */
	private static String getPassword(
		final long time, 
		final String secretKey
	) throws InvalidKeyException, NoSuchAlgorithmException {
		return getDigitsFromHash(
			generateHash(secretKey, time),
			6
		);
	}
	
    /**
     * Generate a HMAC-SHA1 hash of the time.
     * 
     * @throws NoSuchAlgorithmException 
     * @throws InvalidKeyException 
     */
    private static byte[] generateHash(
    	String privateKey, 
    	long time
    ) throws NoSuchAlgorithmException, InvalidKeyException {
        byte[] data = new byte[8];
        long value = time;
        for (int i = 8; i-- > 0; value >>>= 8) {
            data[i] = (byte)value;
        }
        Base32 codec = new Base32();
        byte[] decodedKey = codec.decode(privateKey);
        SecretKeySpec signingKey = new SecretKeySpec(decodedKey, "RAW");
        Mac mac = Mac.getInstance("HmacSHA1");
        mac.init(signingKey);
        return mac.doFinal(data);
    }

    /**
     * Get the n-digit hash.
     *
     * @param hash
     * @param digits
     * @return
     */
    private static String getDigitsFromHash(
    	byte[] hash,
    	int digits
    ) {
        int offset = hash[hash.length - 1] & 0xF;
        long truncatedHash = 0;
        for (int i = 0; i < 4; ++i) {
            truncatedHash <<= 8;
            truncatedHash |= (hash[offset + i] & 0xFF);
        }
        truncatedHash &= 0x7FFFFFFF;
        truncatedHash %= Math.pow(10, digits);
        return String.format("%0" + digits + "d", truncatedHash);
    }
    
    /**
     * Generate secret key.
     * 
     * @return
     */
    public static String generateSecretKey(
    ) {
        SecureRandom random = new SecureRandom();
        byte[] bytes = new byte[20];
        random.nextBytes(bytes);
        Base32 base32 = new Base32();
        return base32.encodeToString(bytes);
    }
    
    /**
     * Get session key.
     * 
     * @param providerName
     * @param segmentName
     * @return
     */
    public static String getSessionKey(
    	String providerName,
    	String segmentName
    ) {
    	return TOTP.class.getSimpleName() + "." + providerName + "." + segmentName;    	
    }
    
    /**
     * Testing TOTP.
     * 
     * @param args
     * @throws InvalidKeyException
     * @throws NoSuchAlgorithmException
     */
    public static void main(
    	String[] args
    ) throws InvalidKeyException, NoSuchAlgorithmException {
    	String secretKey = generateSecretKey();
    	System.out.println("secretKey=" + secretKey);
    	String lastCode = null;
    	while (true) {
    	    String code = getPassword(secretKey);
    	    if (!code.equals(lastCode)) {
    	        System.out.println("code=" + code);
    	    }
    	    lastCode = code;
    	    try {
    	        Thread.sleep(10000);
    	    } catch (InterruptedException e) {};
    	}    	
    }
    
}
