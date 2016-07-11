package com.smart.tool.system;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileSystemView;

import org.apache.commons.io.FileUtils;

import com.smart.tool.generate.Action;
import com.smart.tool.generate.Dao;
import com.smart.tool.generate.DaoImpl;
import com.smart.tool.generate.Edit;
import com.smart.tool.generate.Model;
import com.smart.tool.generate.Service;
import com.smart.tool.generate.ServiceImpl;

/**
 * 生成器主类
 * 
 * @author Joe
 */
public class Generator extends JFrame {
	private static final long serialVersionUID = -4045493623647323022L;

	private JLabel fileLabel = null;
	private JTextField fileText = null;
	private BaseButton fileButton = null;
	private JFileChooser fileChooser = null;
	private JLabel tablesLabel = null;
	private JComboBox<?> tablesBox = null;

	private JLabel outFileLabel = null;
	private JTextField outFileText = null;
	private BaseButton outFileButton = null;

	private JPanel panel1 = null;
	private JLabel projectLabel = null;
	private JTextField projectText = null;

	private JLabel moduleLabel = null;
	private JTextField moduleText = null;

	private JLabel modelLabel = null;
	private JTextField modelText = null;
	private JLabel extendsLabel = null;
	private JComboBox<?> extendsBox = null;

	private JLabel serviceLabel = null;
	private JTextField serviceText = null;

	private JLabel serviceImplLabel = null;
	private JTextField ServiceImplText = null;

	private JLabel daoLabel = null;
	private JTextField daoText = null;

	private JLabel daoImplLabel = null;
	private JTextField daoImplText = null;

	private JLabel actionLabel = null;
	private JTextField actionText = null;

	private JLabel listLabel = null;
	private JTextField listText = null;

	private JLabel editLabel = null;
	private JTextField editText = null;

	private BaseButton generateButton = null;

	// 解析器,包括对sql或者是对数据库表字段解析
	private Analyzer analyzer = null;
	// model继承类配置
	private Map<String, Set<String>> persistentMap = null;
	// 数据库链接
	private Connection connection = null;
	private Statement statement = null;
	// 配置文件
	private DbConfig config = null;

	public Generator() {
		try {
			// 初始化继承类MAP
			persistentMap = new LinkedHashMap<String, Set<String>>(2);
			Set<String> set = new HashSet<String>();
			set.add("id");
			persistentMap.put("PersistentObject", set);
			set = new HashSet<String>();
			set.add("id");
			set.add("createBy");
			set.add("createTime");
			set.add("lastUpdateBy");
			set.add("lastUpdateTime");
			persistentMap.put("BasePersistentObject", set);

			// 初始化读取config文件
			config = new DbConfig();

			// 创建数据库连接
			connection = DbManager.getConnection(config.getDriverClassName(), config.getUrl(), config.getUsername(),
					config.getPassword());
			statement = connection.createStatement();

			// 创建显示窗
			init();

			// 绑定窗口关闭事件
			this.addWindowListener(new CloseHandler());
		}
		catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void main(String[] args) {
		Generator editor = new Generator();
		editor.setDefaultCloseOperation(3);
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension ownerSize = new Dimension(680, 520);
		editor.setSize(ownerSize);
		editor.setLocation((screenSize.width - ownerSize.width) / 2, (screenSize.height - ownerSize.height) / 2);
		editor.setVisible(true);
	}

	private void init() throws Exception {
		setTitle("代码生成工具");
		setFont(new Font("宋体", 0, 8));
		setResizable(false);

		JPanel panel = new JPanel();
		panel.setLayout(null);

		if (DbConfig.GENERATE_TYPE_SQL.equals(config.getGenerateType())) {
			this.fileLabel = new JLabel();
			this.fileLabel.setText("Sql文件");
			this.fileLabel.setToolTipText("文件(.sql)");
			this.fileLabel.setBounds(new Rectangle(10, 10, 100, 20));

			this.fileText = new JTextField();
			this.fileText.setEditable(false);
			this.fileText.setToolTipText("文件(.sql)");
			this.fileText.setBounds(new Rectangle(80, 10, 430, 20));

			this.fileButton = new BaseButton();
			this.fileButton.setText("点击选择文件");
			this.fileButton.setBounds(520, 10, 100, 20);
			this.fileButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					selectSqlFile(e);
				}
			});
			panel.add(this.fileLabel);
			panel.add(this.fileText);
			panel.add(this.fileButton);
		}
		else if (DbConfig.GENERATE_TYPE_DB.equals(config.getGenerateType())) {
			this.tablesLabel = new JLabel();
			this.tablesLabel.setText("表名");
			this.tablesLabel.setToolTipText("表名");
			this.tablesLabel.setBounds(new Rectangle(10, 10, 100, 20));

			this.tablesBox = new JComboBox<Object>(DbManager.getTableNameList(connection).toArray());
			this.tablesBox.setSelectedItem(null);
			this.tablesBox.setBounds(new Rectangle(80, 10, 545, 20));
			this.tablesBox.addActionListener(new ActionListener() {
				@Override
				public void actionPerformed(ActionEvent e) {
					analyzer = new Analyzer(statement, ((JComboBox<?>) e.getSource()).getSelectedItem().toString(), config
							.getUrl());
					changeTextValue();
				}
			});
			panel.add(this.tablesLabel);
			panel.add(this.tablesBox);
		}

		this.outFileLabel = new JLabel();
		this.outFileLabel.setText("输出路径");
		this.outFileLabel.setToolTipText("输出路径");
		this.outFileLabel.setBounds(new Rectangle(10, 40, 100, 20));

		this.outFileText = new JTextField();
		this.outFileText.setText(config.getOutPath());
		this.outFileText.setToolTipText("输出路径");
		this.outFileText.setBounds(new Rectangle(80, 40, 430, 20));

		this.outFileButton = new BaseButton();
		this.outFileButton.setText("点击选择文件");
		this.outFileButton.setBounds(520, 40, 100, 20);
		this.outFileButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				selectOutFile(e);
			}
		});
		// TODO

		panel.add(this.outFileLabel);
		panel.add(this.outFileText);
		panel.add(this.outFileButton);

		this.panel1 = new JPanel();
		this.panel1.setLayout(null);
		this.panel1.setBounds(5, 80, 660, 380);
		this.panel1.setBorder(new TitledBorder("生成包及文件名称"));

		this.projectLabel = new JLabel();
		this.projectLabel.setText("项目名称");
		this.projectLabel.setToolTipText("项目名称");
		this.projectLabel.setBounds(new Rectangle(35, 20, 100, 20));
		this.projectText = new JTextField();
		this.projectText.setText(config.getProjectName());
		this.projectText.addKeyListener(new AddKeyHandler());
		this.projectText.setBounds(new Rectangle(120, 20, 500, 20));

		this.moduleLabel = new JLabel();
		this.moduleLabel.setText("模块名称");
		this.moduleLabel.setToolTipText("模块名称");
		this.moduleLabel.setBounds(new Rectangle(35, 50, 100, 20));
		this.moduleText = new JTextField();
		this.moduleText.setText(config.getModuleName());
		this.moduleText.addKeyListener(new AddKeyHandler());
		this.moduleText.setBounds(new Rectangle(120, 50, 500, 20));

		this.modelLabel = new JLabel();
		this.modelLabel.setText("Model");
		this.modelLabel.setToolTipText("Model");
		this.modelLabel.setBounds(new Rectangle(35, 80, 100, 20));
		this.modelText = new JTextField();
		this.modelText.setBounds(new Rectangle(120, 80, 300, 20));

		this.extendsLabel = new JLabel();
		this.extendsLabel.setText("Extends");
		this.extendsLabel.setToolTipText("Extends");
		this.extendsLabel.setBounds(new Rectangle(420, 80, 50, 20));

		this.extendsBox = new JComboBox<Object>(persistentMap.keySet().toArray());
		this.extendsBox.setBounds(new Rectangle(470, 80, 150, 20));

		this.actionLabel = new JLabel();
		this.actionLabel.setText("Action");
		this.actionLabel.setToolTipText("action");
		this.actionLabel.setBounds(new Rectangle(35, 110, 100, 20));
		this.actionText = new JTextField();
		this.actionText.setBounds(new Rectangle(120, 110, 500, 20));

		this.serviceLabel = new JLabel();
		this.serviceLabel.setText("Service");
		this.serviceLabel.setToolTipText("service");
		this.serviceLabel.setBounds(new Rectangle(35, 140, 100, 20));
		this.serviceText = new JTextField();
		this.serviceText.setBounds(new Rectangle(120, 140, 500, 20));

		this.serviceImplLabel = new JLabel();
		this.serviceImplLabel.setText("ServiceImpl");
		this.serviceImplLabel.setToolTipText("ServiceImpl");
		this.serviceImplLabel.setBounds(new Rectangle(35, 170, 100, 20));
		this.ServiceImplText = new JTextField();
		this.ServiceImplText.setBounds(new Rectangle(120, 170, 500, 20));

		this.daoLabel = new JLabel();
		this.daoLabel.setText("Dao");
		this.daoLabel.setToolTipText("dao");
		this.daoLabel.setBounds(new Rectangle(35, 200, 100, 20));
		this.daoText = new JTextField();
		this.daoText.setBounds(new Rectangle(120, 200, 500, 20));

		this.daoImplLabel = new JLabel();
		this.daoImplLabel.setText("DaoImpl");
		this.daoImplLabel.setToolTipText("DaoImpl");
		this.daoImplLabel.setBounds(new Rectangle(35, 230, 100, 20));
		this.daoImplText = new JTextField();
		this.daoImplText.setBounds(new Rectangle(120, 230, 500, 20));

		this.listLabel = new JLabel();
		this.listLabel.setText("列表页(JSP)");
		this.listLabel.setToolTipText("列表页");
		this.listLabel.setBounds(new Rectangle(35, 260, 100, 20));
		this.listText = new JTextField();
		this.listText.setBounds(new Rectangle(120, 260, 500, 20));

		this.editLabel = new JLabel();
		this.editLabel.setText("编辑页(JSP)");
		this.editLabel.setToolTipText("编辑页");
		this.editLabel.setBounds(new Rectangle(35, 290, 100, 20));
		this.editText = new JTextField();
		this.editText.setBounds(new Rectangle(120, 290, 500, 20));

		changeTextValue();

		this.generateButton = new BaseButton();
		this.generateButton.setText("生成");
		this.generateButton.setToolTipText("生成");
		this.generateButton.setBounds(new Rectangle(280, 320, 80, 25));
		this.generateButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				generateFile(e);
			}
		});

		this.panel1.add(this.projectLabel);
		this.panel1.add(this.projectText);
		this.panel1.add(this.moduleLabel);
		this.panel1.add(this.moduleText);
		this.panel1.add(this.modelLabel);
		this.panel1.add(this.modelText);
		this.panel1.add(this.extendsLabel);
		this.panel1.add(this.extendsBox);
		this.panel1.add(this.serviceLabel);
		this.panel1.add(this.serviceText);
		this.panel1.add(this.serviceImplLabel);
		this.panel1.add(this.ServiceImplText);
		this.panel1.add(this.daoLabel);
		this.panel1.add(this.daoText);
		this.panel1.add(this.daoImplLabel);
		this.panel1.add(this.daoImplText);

		this.panel1.add(this.actionLabel);
		this.panel1.add(this.actionText);
		this.panel1.add(this.listLabel);
		this.panel1.add(this.listText);
		this.panel1.add(this.editLabel);
		this.panel1.add(this.editText);

		this.panel1.add(this.generateButton);

		panel.add(this.panel1);

		JTabbedPane tabPane = new JTabbedPane();
		tabPane.addTab("内容", panel);
		getContentPane().add(tabPane);
	}

	private void selectOutFile(ActionEvent e) {
		if (fileChooser == null) {
			fileChooser = new JFileChooser();
			// 默认桌面
			this.fileChooser.setCurrentDirectory(FileSystemView.getFileSystemView().getHomeDirectory());
		}
		// 只选择文件
		fileChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		int temp = fileChooser.showOpenDialog(null);
		fileChooser.setVisible(true);
		if (temp == 0) {
			File selectedFile = fileChooser.getSelectedFile();
			if (selectedFile != null) {
				this.outFileText.setText(selectedFile.getAbsolutePath());
			}
		}
	}

	private void selectSqlFile(ActionEvent e) {
		if (fileChooser == null) {
			fileChooser = new JFileChooser();
			// 默认桌面
			this.fileChooser.setCurrentDirectory(FileSystemView.getFileSystemView().getHomeDirectory());
		}
		// 只选择文件
		fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
		int temp = fileChooser.showOpenDialog(null);
		fileChooser.setVisible(true);
		if (temp == 0) {
			File selectedFile = fileChooser.getSelectedFile();
			if (selectedFile != null) {
				this.fileText.setText(selectedFile.getAbsolutePath());
				try {
					analyzer = new Analyzer(FileUtils.readFileToString(selectedFile, "UTF-8"));
					changeTextValue();
				}
				catch (IOException e1) {
					e1.printStackTrace();
				}
			}
		}
	}

	private void generateFile(ActionEvent e) {
		String basePath = outFileText.getText();
		if (analyzer != null) {
			generateModelFile(basePath);
			generateServiceFile(basePath);
			generateServiceImplFile(basePath);
			generateDaoFile(basePath);
			generateDaoImplFile(basePath);
			generateActionFile(basePath);
			generateListFile(basePath);
			generateEditFile(basePath);
		}
		JOptionPane.showMessageDialog(null, "文件生成完成!");
	}

	private void generateModelFile(String basePath) {
		List<DummyField> allList = analyzer.getFieldList();
		Set<String> excludeFieldSet = persistentMap.get(extendsBox.getSelectedItem());
		List<DummyField> fieldList = new ArrayList<DummyField>();
		boolean containDate = false;
		for (DummyField dumField : allList) {
			if (excludeFieldSet.contains(dumField.getFieldName())) {
				continue;
			}
			if ("Date".equals(dumField.getFieldType())) {
				containDate = true;
			}
			fieldList.add(dumField);
		}
		com.smart.tool.system.FileUtils.createFile(
				basePath,
				getPackageName().replace(".", "/") + "model" + "/" + analyzer.getModelName() + ".java",
				new Model(config.getCompanyName(), projectText.getText(), moduleText.getText(),
						analyzer.getModelName(), fieldList, analyzer.getTableName(), extendsBox.getSelectedItem()
								.toString(), Long.valueOf(StringUtils.getRandom(17)).toString(), analyzer
								.isContainEnable(), containDate, analyzer
								.getTableComment()).getHtml());
	}

	/**
	 * 首字母大写
	 */
	public static String getUpperStr(String str) {
		return str.substring(0, 1).toUpperCase() + str.substring(1, str.length());
	}

	/**
	 * 首字母小写
	 */
	public static String getLowerStr(String str) {
		return str.substring(0, 1).toLowerCase() + str.substring(1, str.length());
	}

	public String getPackageName() {
		return "com." + config.getCompanyName() + "."
				+ (StringUtils.isNotBlank(projectText.getText()) ? projectText.getText() + "." : "")
				+ (StringUtils.isNotBlank(moduleText.getText()) ? moduleText.getText() + "." : "");
	}

	private void generateServiceFile(String basePath) {
		com.smart.tool.system.FileUtils.createFile(
				basePath,
				getPackageName().replace(".", "/") + "service" + "/" + analyzer.getModelName() + "Service" + ".java",
				new Service(config.getCompanyName(), projectText.getText(), moduleText.getText(), analyzer
						.getModelName()).getHtml());
	}

	private void generateServiceImplFile(String basePath) {
		com.smart.tool.system.FileUtils.createFile(basePath, getPackageName().replace(".", "/") + "service" + "/" + "impl"
				+ "/" + analyzer.getModelName() + "ServiceImpl.java", new ServiceImpl(config.getCompanyName(),
				projectText.getText(), moduleText.getText(), analyzer.getModelName()).getHtml());
	}

	private void generateDaoFile(String basePath) {
		com.smart.tool.system.FileUtils.createFile(basePath,
				getPackageName().replace(".", "/") + "dao" + "/" + analyzer.getModelName() + "Dao.java",
				new Dao(config.getCompanyName(), projectText.getText(), moduleText.getText(), analyzer.getModelName())
						.getHtml());
	}

	private void generateDaoImplFile(String basePath) {
		com.smart.tool.system.FileUtils.createFile(basePath, getPackageName().replace(".", "/") + "dao" + "/" + "impl" + "/"
				+ analyzer.getModelName() + "DaoImpl.java", new DaoImpl(config.getCompanyName(), projectText.getText(),
				moduleText.getText(), analyzer.getModelName()).getHtml());
	}

	private void generateActionFile(String basePath) {
		com.smart.tool.system.FileUtils.createFile(
				basePath,
				getPackageName().replace(".", "/") + "action" + "/" + analyzer.getModelName() + "Action.java",
				new Action(config.getCompanyName(), projectText.getText(), moduleText.getText(), analyzer
						.getModelName(), analyzer.isContainEnable()).getHtml());
	}

	private void generateListFile(String basePath) {
		if (StringUtils.isNotBlank(this.listText.getText())) {
			List<DummyField> allList = analyzer.getFieldList();
			Set<String> excludeFieldSet = persistentMap.get(extendsBox.getSelectedItem());
			List<DummyField> fieldList = new ArrayList<DummyField>();
			for (DummyField dumField : allList) {
				if (excludeFieldSet.contains(dumField.getFieldName())) {
					continue;
				}
				fieldList.add(dumField);
			}
			
			
			File f = new File(this.listText.getText());
			File f1 = f.getParentFile();
			File f2 = f1.getParentFile();
			com.smart.tool.system.FileUtils.createFile(
					f2.getAbsolutePath() + "\\" + f1.getName(),
					f.getName(),
					new com.smart.tool.generate.List(analyzer.getTableComment(), analyzer.getModelName(), analyzer
							.isContainEnable(), Analyzer.ENABLE_NAME, fieldList).getHtml());
		}
	}
	
	private void generateEditFile(String basePath) {
		if (StringUtils.isNotBlank(this.editText.getText())) {
			List<DummyField> allList = analyzer.getFieldList();
			Set<String> excludeFieldSet = persistentMap.get(extendsBox.getSelectedItem());
			List<DummyField> fieldList = new ArrayList<DummyField>();
			for (DummyField dumField : allList) {
				if (excludeFieldSet.contains(dumField.getFieldName())) {
					continue;
				}
				fieldList.add(dumField);
			}
			
			
			File f = new File(this.editText.getText());
			File f1 = f.getParentFile();
			File f2 = f1.getParentFile();
			com.smart.tool.system.FileUtils.createFile(
					f2.getAbsolutePath() + "\\" + f1.getName(),
					f.getName(),
					new Edit(analyzer.getTableComment(), analyzer.getModelName(), analyzer
							.isContainEnable(), fieldList).getHtml());
		}
	}

	private void changeTextValue() {
		this.modelText.setText("com." + config.getCompanyName() + "."
				+ (StringUtils.isNotBlank(projectText.getText()) ? projectText.getText() + "." : "")
				+ (StringUtils.isNotBlank(moduleText.getText()) ? moduleText.getText() + "." : "") + "model"
				+ (analyzer != null ? "." + analyzer.getModelName() + ".java" : ""));
		this.serviceText.setText("com." + config.getCompanyName() + "."
				+ (StringUtils.isNotBlank(projectText.getText()) ? projectText.getText() + "." : "")
				+ (StringUtils.isNotBlank(moduleText.getText()) ? moduleText.getText() + "." : "") + "service"
				+ (analyzer != null ? "." + analyzer.getModelName() + "Service.java" : ""));
		this.ServiceImplText.setText("com." + config.getCompanyName() + "."
				+ (StringUtils.isNotBlank(projectText.getText()) ? projectText.getText() + "." : "")
				+ (StringUtils.isNotBlank(moduleText.getText()) ? moduleText.getText() + "." : "") + "service.impl"
				+ (analyzer != null ? "." + analyzer.getModelName() + "ServiceImpl.java" : ""));
		this.daoText.setText("com." + config.getCompanyName() + "."
				+ (StringUtils.isNotBlank(projectText.getText()) ? projectText.getText() + "." : "")
				+ (StringUtils.isNotBlank(moduleText.getText()) ? moduleText.getText() + "." : "") + "dao"
				+ (analyzer != null ? "." + analyzer.getModelName() + "Dao.java" : ""));
		this.daoImplText.setText("com." + config.getCompanyName() + "."
				+ (StringUtils.isNotBlank(projectText.getText()) ? projectText.getText() + "." : "")
				+ (StringUtils.isNotBlank(moduleText.getText()) ? moduleText.getText() + "." : "") + "dao.impl"
				+ (analyzer != null ? "." + analyzer.getModelName() + "DaoImpl.java" : ""));
		this.actionText.setText("com." + config.getCompanyName() + "."
				+ (StringUtils.isNotBlank(projectText.getText()) ? projectText.getText() + "." : "")
				+ (StringUtils.isNotBlank(moduleText.getText()) ? moduleText.getText() + "." : "") + "action"
				+ (analyzer != null ? "." + analyzer.getModelName() + "Action.java" : ""));

		File parent = new File(outFileText.getText()).getParentFile();
		if (parent != null) {
			File file = new File(parent.getAbsolutePath() + "\\WebRoot\\WEB-INF");
			if (file != null) {
				this.listText
						.setText(file.getAbsolutePath()
								+ "\\"
								+ (analyzer != null ? moduleText.getText() + "\\" + analyzer.getLowerModelName()
										+ ".jsp" : ""));
				this.editText.setText(file.getAbsolutePath()
						+ "\\"
						+ (analyzer != null ? moduleText.getText() + "\\" + analyzer.getLowerModelName() + "Edit.jsp"
								: ""));
			}
		}
	}

	private class AddKeyHandler implements KeyListener {
		private AddKeyHandler() {
		}

		public void keyPressed(KeyEvent e) {
		}

		public void keyReleased(KeyEvent e) {
			changeTextValue();
		}

		public void keyTyped(KeyEvent e) {
		}
	}

	private class CloseHandler extends WindowAdapter {
		public void windowClosing(WindowEvent event) {
			try {
				if (statement != null)
					statement.close();
				if (connection != null)
					connection.close();
			}
			catch (Exception e) {
				JOptionPane.showMessageDialog(null, "数据库连接关闭失败!");
			}
			statement = null;
			connection = null;
		}
	}
}
