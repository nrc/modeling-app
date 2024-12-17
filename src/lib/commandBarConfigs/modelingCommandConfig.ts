import { Models } from '@kittycad/lib'
import { angleLengthInfo } from 'components/Toolbar/setAngleLength'
import { transformAstSketchLines } from 'lang/std/sketchcombos'
import { PathToNode } from 'lang/wasm'
import { StateMachineCommandSetConfig, KclCommandValue } from 'lib/commandTypes'
import { KCL_DEFAULT_LENGTH, KCL_DEFAULT_DEGREE } from 'lib/constants'
import { components } from 'lib/machine-api'
import { Selections } from 'lib/selections'
import { kclManager } from 'lib/singletons'
import { err } from 'lib/trap'
import { modelingMachine, SketchTool } from 'machines/modelingMachine'
import { loftValidator, revolveAxisValidator } from './validators'

type OutputFormat = Models['OutputFormat_type']
type OutputTypeKey = OutputFormat['type']
type ExtractStorageTypes<T> = T extends { storage: infer U } ? U : never
type StorageUnion = ExtractStorageTypes<OutputFormat>

export const EXTRUSION_RESULTS = [
  'new',
  'add',
  'subtract',
  'intersect',
] as const

export type ModelingCommandSchema = {
  'Enter sketch': {}
  Export: {
    type: OutputTypeKey
    storage?: StorageUnion
  }
  Make: {
    machine: components['schemas']['MachineInfoResponse']
  }
  Extrude: {
    selection: Selections // & { type: 'face' } would be cool to lock that down
    // result: (typeof EXTRUSION_RESULTS)[number]
    distance: KclCommandValue
  }
  Loft: {
    selection: Selections
  }
  Shell: {
    selection: Selections
    thickness: KclCommandValue
  }
  Revolve: {
    selection: Selections
    angle: KclCommandValue
    axis: Selections
  }
  Fillet: {
    // todo
    selection: Selections
    radius: KclCommandValue
  }
  'Offset plane': {
    plane: Selections
    distance: KclCommandValue
  }
  'change tool': {
    tool: SketchTool
  }
  'Constrain length': {
    selection: Selections
    length: KclCommandValue
  }
  'Constrain with named value': {
    currentValue: {
      valueText: string
      pathToNode: PathToNode
      variableName: string
    }
    namedValue: KclCommandValue
  }
  'Text-to-CAD': {
    prompt: string
  }
}

export const modelingMachineCommandConfig: StateMachineCommandSetConfig<
  typeof modelingMachine,
  ModelingCommandSchema
> = {
  'Enter sketch': {
    description: 'Enter sketch mode.',
    icon: 'sketch',
  },
  'change tool': [
    {
      description: 'Start drawing straight lines.',
      icon: 'line',
      displayName: 'Line',
      args: {
        tool: {
          defaultValue: 'line',
          required: true,
          skip: true,
          inputType: 'string',
        },
      },
    },
    {
      description: 'Start drawing an arc tangent to the current segment.',
      icon: 'arc',
      displayName: 'Tangential Arc',
      args: {
        tool: {
          defaultValue: 'tangentialArc',
          required: true,
          skip: true,
          inputType: 'string',
        },
      },
    },
    {
      description: 'Start drawing a rectangle.',
      icon: 'rectangle',
      displayName: 'Rectangle',
      args: {
        tool: {
          defaultValue: 'rectangle',
          required: true,
          skip: true,
          inputType: 'string',
        },
      },
    },
  ],
  Export: {
    description: 'Export the current model.',
    icon: 'floppyDiskArrow',
    needsReview: true,
    args: {
      type: {
        inputType: 'options',
        defaultValue: 'gltf',
        required: true,
        options: [
          { name: 'glTF', isCurrent: true, value: 'gltf' },
          { name: 'OBJ', isCurrent: false, value: 'obj' },
          { name: 'STL', isCurrent: false, value: 'stl' },
          { name: 'STEP', isCurrent: false, value: 'step' },
          { name: 'PLY', isCurrent: false, value: 'ply' },
        ],
      },
      storage: {
        inputType: 'options',
        defaultValue: (c) => {
          switch (c.argumentsToSubmit.type) {
            case 'gltf':
              return 'embedded'
            case 'stl':
              return 'ascii'
            case 'ply':
              return 'ascii'
            default:
              return undefined
          }
        },
        skip: true,
        required: (commandContext) =>
          ['gltf', 'stl', 'ply'].includes(
            commandContext.argumentsToSubmit.type as string
          ),
        options: (commandContext) => {
          const type = commandContext.argumentsToSubmit.type as
            | OutputTypeKey
            | undefined

          switch (type) {
            case 'gltf':
              return [
                { name: 'embedded', isCurrent: true, value: 'embedded' },
                { name: 'binary', isCurrent: false, value: 'binary' },
                { name: 'standard', isCurrent: false, value: 'standard' },
              ]
            case 'stl':
              return [
                { name: 'binary', isCurrent: false, value: 'binary' },
                { name: 'ascii', isCurrent: true, value: 'ascii' },
              ]
            case 'ply':
              return [
                { name: 'ascii', isCurrent: true, value: 'ascii' },
                {
                  name: 'binary_big_endian',
                  isCurrent: false,
                  value: 'binary_big_endian',
                },
                {
                  name: 'binary_little_endian',
                  isCurrent: false,
                  value: 'binary_little_endian',
                },
              ]
            default:
              return []
          }
        },
      },
    },
  },
  Make: {
    hide: 'web',
    displayName: 'Make',
    description:
      'Export the current part and send to a 3D printer on the network.',
    icon: 'printer3d',
    needsReview: true,
    args: {
      machine: {
        inputType: 'options',
        required: true,
        valueSummary: (machine: components['schemas']['MachineInfoResponse']) =>
          machine.make_model.model ||
          machine.make_model.manufacturer ||
          'Unknown Machine',
        options: (commandBarContext) => {
          return Object.values(
            commandBarContext.machineManager?.machines || []
          ).map((machine: components['schemas']['MachineInfoResponse']) => ({
            name:
              `${machine.id} (${
                machine.make_model.model || machine.make_model.manufacturer
              }) (${machine.state.state})` +
              (machine.hardware_configuration &&
              machine.hardware_configuration.type !== 'none' &&
              machine.hardware_configuration.config.nozzle_diameter
                ? ` - Nozzle Diameter: ${machine.hardware_configuration.config.nozzle_diameter}`
                : '') +
              (machine.hardware_configuration &&
              machine.hardware_configuration.type !== 'none' &&
              machine.hardware_configuration.config.filaments &&
              machine.hardware_configuration.config.filaments[0]
                ? ` - ${
                    machine.hardware_configuration.config.filaments[0].name
                  } #${
                    machine.hardware_configuration.config &&
                    machine.hardware_configuration.config.filaments[0].color?.slice(
                      0,
                      6
                    )
                  }`
                : ''),
            isCurrent: false,
            disabled: machine.state.state !== 'idle',
            value: machine,
          }))
        },
        defaultValue: (commandBarContext) => {
          return Object.values(
            commandBarContext.machineManager.machines || []
          )[0] as components['schemas']['MachineInfoResponse']
        },
      },
    },
  },
  Extrude: {
    description: 'Pull a sketch into 3D along its normal or perpendicular.',
    icon: 'extrude',
    needsReview: true,
    args: {
      selection: {
        inputType: 'selection',
        selectionTypes: ['solid2D', 'segment'],
        multiple: false, // TODO: multiple selection
        required: true,
        skip: true,
      },
      // result: {
      //   inputType: 'options',
      //   defaultValue: 'add',
      //   skip: true,
      //   required: true,
      //   options: EXTRUSION_RESULTS.map((r) => ({
      //     name: r,
      //     isCurrent: r === 'add',
      //     value: r,
      //   })),
      // },
      distance: {
        inputType: 'kcl',
        defaultValue: KCL_DEFAULT_LENGTH,
        required: true,
      },
    },
  },
  Loft: {
    description: 'Create a 3D body by blending between two or more sketches',
    icon: 'loft',
    needsReview: true,
    args: {
      selection: {
        inputType: 'selection',
        selectionTypes: ['solid2D'],
        multiple: true,
        required: true,
        skip: false,
        validation: loftValidator,
      },
    },
  },
  Shell: {
    description: 'Hollow out a 3D solid.',
    icon: 'shell',
    needsReview: true,
    args: {
      selection: {
        inputType: 'selection',
        selectionTypes: ['cap', 'wall'],
        multiple: true,
        required: true,
        skip: false,
      },
      thickness: {
        inputType: 'kcl',
        defaultValue: KCL_DEFAULT_LENGTH,
        required: true,
      },
    },
  },
  // TODO: Update this configuration, copied from extrude for MVP of revolve, specifically the args.selection
  Revolve: {
    description: 'Create a 3D body by rotating a sketch region about an axis.',
    icon: 'revolve',
    needsReview: true,
    args: {
      selection: {
        inputType: 'selection',
        selectionTypes: ['solid2D', 'segment'],
        multiple: false, // TODO: multiple selection
        required: true,
        skip: true,
      },
      axis: {
        required: true,
        inputType: 'selection',
        selectionTypes: ['segment', 'sweepEdge', 'edgeCutEdge'],
        multiple: false,
        validation: revolveAxisValidator,
      },
      angle: {
        inputType: 'kcl',
        defaultValue: KCL_DEFAULT_DEGREE,
        required: true,
      },
    },
  },
  'Offset plane': {
    description: 'Offset a plane.',
    icon: 'plane',
    args: {
      plane: {
        inputType: 'selection',
        selectionTypes: ['plane'],
        multiple: false,
        required: true,
        skip: true,
      },
      distance: {
        inputType: 'kcl',
        defaultValue: KCL_DEFAULT_LENGTH,
        required: true,
      },
    },
  },
  Fillet: {
    description: 'Fillet edge',
    icon: 'fillet',
    status: 'development',
    needsReview: true,
    args: {
      selection: {
        inputType: 'selection',
        selectionTypes: ['segment', 'sweepEdge', 'edgeCutEdge'],
        multiple: true,
        required: true,
        skip: false,
        warningMessage:
          'Fillets cannot touch other fillets yet. This is under development.',
      },
      radius: {
        inputType: 'kcl',
        defaultValue: KCL_DEFAULT_LENGTH,
        required: true,
      },
    },
  },
  'Constrain length': {
    description: 'Constrain the length of one or more segments.',
    icon: 'dimension',
    args: {
      selection: {
        inputType: 'selection',
        selectionTypes: ['segment'],
        multiple: false,
        required: true,
        skip: true,
      },
      length: {
        inputType: 'kcl',
        required: true,
        createVariableByDefault: true,
        defaultValue(_, machineContext) {
          const selectionRanges = machineContext?.selectionRanges
          if (!selectionRanges) return KCL_DEFAULT_LENGTH
          const angleLength = angleLengthInfo({
            selectionRanges,
            angleOrLength: 'setLength',
          })
          if (err(angleLength)) return KCL_DEFAULT_LENGTH
          const { transforms } = angleLength

          // QUESTION: is it okay to reference kclManager here? will its state be up to date?
          const sketched = transformAstSketchLines({
            ast: structuredClone(kclManager.ast),
            selectionRanges,
            transformInfos: transforms,
            programMemory: kclManager.programMemory,
            referenceSegName: '',
          })
          if (err(sketched)) return KCL_DEFAULT_LENGTH
          const { valueUsedInTransform } = sketched
          return valueUsedInTransform?.toString() || KCL_DEFAULT_LENGTH
        },
      },
    },
  },
  'Constrain with named value': {
    description: 'Constrain a value by making it a named constant.',
    icon: 'make-variable',
    args: {
      currentValue: {
        description:
          'Path to the node in the AST to constrain. This is never shown to the user.',
        inputType: 'text',
        required: false,
        skip: true,
      },
      namedValue: {
        inputType: 'kcl',
        required: true,
        createVariableByDefault: true,
        variableName(commandBarContext, machineContext) {
          const { currentValue } = commandBarContext.argumentsToSubmit
          if (
            !currentValue ||
            !(currentValue instanceof Object) ||
            !('variableName' in currentValue) ||
            typeof currentValue.variableName !== 'string'
          ) {
            return 'value'
          }
          return currentValue.variableName
        },
        defaultValue: (commandBarContext) => {
          const { currentValue } = commandBarContext.argumentsToSubmit
          if (
            !currentValue ||
            !(currentValue instanceof Object) ||
            !('valueText' in currentValue) ||
            typeof currentValue.valueText !== 'string'
          ) {
            return KCL_DEFAULT_LENGTH
          }
          return currentValue.valueText
        },
      },
    },
  },
  'Text-to-CAD': {
    description: 'Use the Zoo Text-to-CAD API to generate part starters.',
    icon: 'chat',
    args: {
      prompt: {
        inputType: 'text',
        required: true,
      },
    },
  },
}
