package com.smart.tool.system;

import java.awt.Font;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.sql.Connection;
import java.sql.Statement;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import javax.swing.JCheckBox;
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

/**
 * 生成器窗口类
 * 
 * @author Joe
 */
public abstract class BaseFrame extends JFrame {

	private static final long serialVersionUID = -4045493623647323022L;

	private JFileChooser fileChooser = null;
	private JLabel tablesLabel = null;
	private JComboBox<?> tablesBox = null;

	private JLabel outFileLabel = null;
	protected JTextField outFileText = null;
	private BaseButton outFileButton = null;

	private JPanel panel1 = null;
	private JLabel projectLabel = null;
	protected JTextField projectText = null;

	private JLabel moduleLabel = null;
	protected JTextField moduleText = null;

	private JLabel modelLabel = null;
	protected JTextField modelText = null;
	private JLabel extendsLabel = null;
	protected JComboBox<?> extendsBox = null;

	private JLabel serviceLabel = null;
	protected JTextField serviceText = null;

	private JLabel serviceImplLabel = null;
	protected JTextField ServiceImplText = null;

	private JLabel daoLabel = null;
	protected JTextField daoText = null;

	private JLabel modelXmlLabel = null;
	protected JTextField modelXmlText = null;
	
	protected JCheckBox adminCheckBox = null;

	private JLabel controllerLabel = null;
	protected JTextField controllerText = null;

	private JLabel listLabel = null;
	protected JTextField listText = null;

	private JLabel editLabel = null;
	protected JTextField editText = null;

	private BaseButton generateButton = null;

	// 解析器,包括对sql或者是对数据库表字段解析
	protected Analyzer analyzer = null;
	// model继承类配置
	protected Map<String, Set<String>> persistentMap = null;
	// 数据库链接
	protected Connection connection = null;
	protected Statement statement = null;
	// 配置文件
	protected DbConfig config = null;

	public BaseFrame() {
		
		// 初始化持久对象Map
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
		try {
			statement = connection.createStatement();
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		
		// 创建显示窗
		init();

		// 绑定窗口关闭事件
		this.addWindowListener(new CloseHandler());
	}

	private void init() {
		setTitle("代码生成工具");
		setFont(new Font("宋体", 0, 8));
		setResizable(false);

		JPanel panel = new JPanel();
		panel.setLayout(null);

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
				analyzer = new Analyzer(statement, ((JComboBox<?>) e.getSource()).getSelectedItem().toString(),
						config.getUrl());
				changeTextValue();
			}
		});
		panel.add(this.tablesLabel);
		panel.add(this.tablesBox);

		this.outFileLabel = new JLabel();
		this.outFileLabel.setText("输出路径");
		this.outFileLabel.setToolTipText("输出路径");
		this.outFileLabel.setBounds(new Rectangle(10, 40, 100, 20));

		this.outFileText = new JTextField();
		this.outFileText.setText(config.getOutPath());
		this.outFileText.setEditable(false);
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

		panel.add(this.outFileLabel);
		panel.add(this.outFileText);
		panel.add(this.outFileButton);

		this.panel1 = new JPanel();
		this.panel1.setLayout(null);
		this.panel1.setBounds(5, 80, 660, 390);
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

		this.controllerLabel = new JLabel();
		this.controllerLabel.setText("Controller");
		this.controllerLabel.setToolTipText("controller");
		this.controllerLabel.setBounds(new Rectangle(35, 110, 100, 20));
		this.controllerText = new JTextField();
		this.controllerText.setBounds(new Rectangle(120, 110, 500, 20));

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

		this.modelXmlLabel = new JLabel();
		this.modelXmlLabel.setText("Mapper");
		this.modelXmlLabel.setToolTipText("Mapper");
		this.modelXmlLabel.setBounds(new Rectangle(35, 230, 100, 20));
		this.modelXmlText = new JTextField();
		this.modelXmlText.setBounds(new Rectangle(120, 230, 500, 20));
		
		this.adminCheckBox = new JCheckBox("后台管理页");
		this.adminCheckBox.setBounds(new Rectangle(120, 260, 500, 20));
		this.adminCheckBox.setSelected(true);
		this.adminCheckBox.addItemListener(new ItemListener() {
            JCheckBox checkBox;
            public void itemStateChanged(ItemEvent e) {
				checkBox = (JCheckBox) e.getSource();
				if (checkBox.isSelected()) {
					changeTextValue();
				}
				else {
					changeTextValue();
				}
            }
        });

		this.listLabel = new JLabel();
		this.listLabel.setText("列表页(JSP)");
		this.listLabel.setToolTipText("列表页");
		this.listLabel.setBounds(new Rectangle(35, 290, 100, 20));
		this.listText = new JTextField();
		this.listText.setBounds(new Rectangle(120, 290, 500, 20));

		this.editLabel = new JLabel();
		this.editLabel.setText("编辑页(JSP)");
		this.editLabel.setToolTipText("编辑页");
		this.editLabel.setBounds(new Rectangle(35, 320, 100, 20));
		this.editText = new JTextField();
		this.editText.setBounds(new Rectangle(120, 320, 500, 20));

		changeTextValue();

		this.generateButton = new BaseButton();
		this.generateButton.setText("生成");
		this.generateButton.setToolTipText("生成");
		this.generateButton.setBounds(new Rectangle(280, 350, 80, 25));
		this.generateButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				generateEvent(e);
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
		this.panel1.add(this.modelXmlLabel);
		this.panel1.add(this.modelXmlText);
		this.panel1.add(this.controllerLabel);
		this.panel1.add(this.controllerText);
		
		this.panel1.add(this.adminCheckBox);
		
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

	/**
	 * 生成按钮事件
	 */
	private void generateEvent(ActionEvent e) {
		if (analyzer != null) {
			generateFile(outFileText.getText() + "/");
		}
		JOptionPane.showMessageDialog(null, "文件生成完成!");
	}
	
	protected abstract void generateFile(String basePath);

	protected abstract void changeTextValue();

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
