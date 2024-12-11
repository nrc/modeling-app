export const APP_NAME = 'Modeling App'
/** Search string in new project names to increment as an index */
export const INDEX_IDENTIFIER = '$n'
/** The maximum number of 0's to pad a default project name's index with */
export const MAX_PADDING = 7
/** The default name for a newly-created project.
 * This is used as a template for new projects, with $nnn being replaced by an index
 * This is available for users to edit as a setting.
 */
export const DEFAULT_PROJECT_NAME = 'project-$nnn'
export const DEFAULT_PROJECT_KCL_FILE = 'main.kcl'
/** Name given the temporary "project" in the browser version of the app */
export const BROWSER_PROJECT_NAME = 'browser'
/** Name given the temporary file in the browser version of the app */
export const BROWSER_FILE_NAME = 'main'
/**
 * The default name of the project in Desktop.
 * This is prefixed by the Documents directory path.
 */
export const PROJECT_FOLDER = 'zoo-modeling-app-projects'
/**
 * File extension for Modeling App's files, which are written in kcl
 * @link - https://zoo.dev/docs/kcl
 * */
export const FILE_EXT = '.kcl'
/** Default file to open when a project is opened */
export const PROJECT_ENTRYPOINT = `main${FILE_EXT}` as const
/** Thumbnail file name */
export const PROJECT_IMAGE_NAME = `main.jpg` as const
/** The localStorage key for last-opened projects */
export const FILE_PERSIST_KEY = `${PROJECT_FOLDER}-last-opened` as const
/** The default name given to new kcl files in a project */
export const DEFAULT_FILE_NAME = 'Untitled'
/** The file endings that will appear in
 * the file explorer if found in a project directory */
export const RELEVANT_FILE_TYPES = [
  'kcl',
  'fbx',
  'gltf',
  'glb',
  'obj',
  'ply',
  'step',
  'stl',
] as const
/** The default name for a tutorial project */
export const ONBOARDING_PROJECT_NAME = 'Tutorial Project $nn'
/**
 * The default starting constant name for various modeling operations.
 * These are used to generate unique names for new objects.
 * */
export const KCL_DEFAULT_CONSTANT_PREFIXES = {
  SKETCH: 'sketch',
  EXTRUDE: 'extrude',
  LOFT: 'loft',
  SHELL: 'shell',
  SEGMENT: 'seg',
  REVOLVE: 'revolve',
  PLANE: 'plane',
} as const
/** The default KCL length expression */
export const KCL_DEFAULT_LENGTH = `5`

/** The default KCL degree expression */
export const KCL_DEFAULT_DEGREE = `360`

/** localStorage key for the playwright test-specific app settings file */
export const TEST_SETTINGS_FILE_KEY = 'playwright-test-settings'

export const DEFAULT_HOST = 'https://api.zoo.dev'
export const SETTINGS_FILE_NAME = 'settings.toml'
export const TOKEN_FILE_NAME = 'token.txt'
export const PROJECT_SETTINGS_FILE_NAME = 'project.toml'
export const COOKIE_NAME = '__Secure-next-auth.session-token'
export const TELEMETRY_FILE_NAME = 'boot.txt'
export const TELEMETRY_RAW_FILE_NAME = 'raw-metrics.txt'

/** localStorage key to determine if we're in Playwright tests */
export const PLAYWRIGHT_KEY = 'playwright'

/** Custom error message to match when rejectAllModelCommands is called
 * allows us to match if the execution of executeAst was interrupted */
export const EXECUTE_AST_INTERRUPT_ERROR_MESSAGE =
  'Force interrupt, executionIsStale, new AST requested'

/** The messages that appear for exporting toasts */
export const EXPORT_TOAST_MESSAGES = {
  START: 'Exporting...',
  SUCCESS: 'Exported successfully',
  FAILED: 'Export failed',
}

/** The messages that appear for "make" command toasts */
export const MAKE_TOAST_MESSAGES = {
  START: 'Starting print...',
  NO_MACHINES: 'No machines available',
  NO_MACHINE_API_IP: 'No machine api ip available',
  NO_CURRENT_MACHINE: 'No current machine available',
  NO_MACHINE_ID: 'No machine id available',
  NO_NAME: 'No name provided',
  ERROR_STARTING_PRINT: 'Error while starting print',
  SUCCESS: 'Started print successfully',
}

/** The URL for the KCL samples manifest files */
export const KCL_SAMPLES_MANIFEST_URLS = {
  remote:
    'https://raw.githubusercontent.com/KittyCAD/kcl-samples/main/manifest.json',
  localFallback: '/kcl-samples-manifest-fallback.json',
} as const

/** Toast id for the app auto-updater toast */
export const AUTO_UPDATER_TOAST_ID = 'auto-updater-toast'

/** Local sketch axis values in KCL for operations, it could either be 'X' or 'Y' */
export const KCL_AXIS_X = 'X'
export const KCL_AXIS_Y = 'Y'
export const KCL_AXIS_NEG_X = '-X'
export const KCL_AXIS_NEG_Y = '-Y'
export const KCL_DEFAULT_AXIS = 'X'

export enum AxisNames {
  X = 'x',
  Y = 'y',
  Z = 'z',
  NEG_X = '-x',
  NEG_Y = '-y',
  NEG_Z = '-z',
}
/** Semantic names of views from AxisNames */
export const VIEW_NAMES_SEMANTIC = {
  [AxisNames.X]: 'Right',
  [AxisNames.Y]: 'Back',
  [AxisNames.Z]: 'Top',
  [AxisNames.NEG_X]: 'Left',
  [AxisNames.NEG_Y]: 'Front',
  [AxisNames.NEG_Z]: 'Bottom',
} as const
