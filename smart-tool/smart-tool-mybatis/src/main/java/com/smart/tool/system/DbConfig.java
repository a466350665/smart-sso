package com.smart.tool.system;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.Properties;

/**
 * 配置文件对应实体
 * 
 * @author Joe
 */
public class DbConfig {
	public static String GENERATE_TYPE_SQL = "sql";
	public static String GENERATE_TYPE_DB = "db";

	private String outPath;
	private String companyName;
	private String projectName;
	private String moduleName;

	private String driverClassName;
	private String url;
	private String username;
	private String password;

	public DbConfig() {
		Properties props = new Properties();
		try {
			InputStream in = new FileInputStream(new File("config.properties"));
			props.load(in);

			outPath = props.getProperty("default.outPath");
			companyName = props.getProperty("default.companyName");
			projectName = props.getProperty("default.projectName");
			moduleName = props.getProperty("default.moduleName");

			driverClassName = props.getProperty("db.driverClassName");
			url = props.getProperty("db.url");
			username = props.getProperty("db.username");
			password = props.getProperty("db.password");
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}

	public String getOutPath() {
		return outPath;
	}

	public void setOutPath(String outPath) {
		this.outPath = outPath;
	}

	public String getCompanyName() {
		return companyName;
	}

	public void setCompanyName(String companyName) {
		this.companyName = companyName;
	}

	public String getProjectName() {
		return projectName;
	}

	public void setProjectName(String projectName) {
		this.projectName = projectName;
	}

	public String getModuleName() {
		return moduleName;
	}

	public void setModuleName(String moduleName) {
		this.moduleName = moduleName;
	}

	public String getDriverClassName() {
		return driverClassName;
	}

	public void setDriverClassName(String driverClassName) {
		this.driverClassName = driverClassName;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}
}
