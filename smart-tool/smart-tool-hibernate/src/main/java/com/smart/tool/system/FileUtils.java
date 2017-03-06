package com.smart.tool.system;

import java.io.File;
import java.io.IOException;

import javax.swing.JOptionPane;

/**
 * 文件公用类
 */
public class FileUtils {

	public static File loopCreateFile(String basePath, String nestFile) throws IOException {
		File floder = new File(basePath);
		if (!floder.exists()) {
			floder.mkdirs();
		}
		String path[] = nestFile.split("/");

		String currentPath = basePath;
		for (int i = 0; i < path.length; i++) {
			currentPath += "/" + path[i];
			File f = new File(currentPath);
			if (currentPath.endsWith(".java") || currentPath.endsWith(".jsp")) {
				if (!f.exists()) {
					f.createNewFile();
				}
				return f;
			}
			else {
				if (!f.exists())
					f.mkdir();
			}
		}
		return null;
	}

	public static void createFile(String basePath, String subPath, String content) {
		try {
			File file = loopCreateFile(basePath, subPath);
			org.apache.commons.io.FileUtils.writeStringToFile(file, content, "UTF-8");
		}
		catch (IOException e) {
			e.printStackTrace();
			JOptionPane.showMessageDialog(null, "文件创建失败!");
		}
	}
}
