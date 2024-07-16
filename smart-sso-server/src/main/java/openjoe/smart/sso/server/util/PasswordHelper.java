package openjoe.smart.sso.server.util;

import openjoe.smart.sso.server.enums.ErrorCodeEnum;
import openjoe.smart.stage.exception.ApplicationException;
import org.springframework.util.StringUtils;

import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * 密码加密实现提供者
 * 
 * @author Joe
 */
public class PasswordHelper {

	private static final String SUFFIX = "`1qazx";

	/**
	 * 加密
	 * 
	 * @param password
	 *            Md5密码
	 * @return
	 */
	public static String encrypt(String password) {
		if (StringUtils.isEmpty(password)) {
			throw new ApplicationException(ErrorCodeEnum.E1001);
		}
		try {
			return md5(new StringBuilder(password).append(SUFFIX).toString());
		}
		catch (Exception e) {
			throw new ApplicationException(ErrorCodeEnum.E1002);
		}
	}

	private static String md5(String str) {
		String password = null;
		try {
			// 生成一个MD5加密计算摘要
			MessageDigest md = MessageDigest.getInstance("MD5");
			// 计算md5函数
			md.update(str.getBytes());
			// digest()最后确定返回md5 hash值，返回值为8为字符串。因为md5 hash值是16位的hex值，实际上就是8位的字符
			// BigInteger函数则将8位的字符串转换成16位hex值，用字符串来表示；得到字符串形式的hash值
			password = new BigInteger(1, md.digest()).toString(16);
		}
		catch (NoSuchAlgorithmException e) {
			e.printStackTrace();
		}
		return password;
	}

	public static void main(String[] args) {
		System.err.println("加密        后:" + encrypt("123456"));
	}
}
