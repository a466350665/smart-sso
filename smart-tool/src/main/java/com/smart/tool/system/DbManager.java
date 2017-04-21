package com.smart.tool.system;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JOptionPane;

/**
 * 数据库连接管理
 * 
 * @author Joe
 */
public class DbManager {
	public static Connection getConnection(String driver, String url, String userName, String password) {
		Connection con = null;
		try {
			Class.forName(driver);
			con = DriverManager.getConnection(url, userName, password);
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(null, "数据库连接失败!");
		}
		return con;
	}

	public static List<String> getTableNameList(Connection con) {
		List<String> tableNames = new ArrayList<String>();
		try {
			DatabaseMetaData meta = con.getMetaData();
			ResultSet rs = meta.getTables(null, null, null, new String[] { "TABLE" });
			while (rs.next()) {
				tableNames.add(rs.getString(3));
			}
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(null, "获取表名失败!");
		}
		return tableNames;
	}
}
