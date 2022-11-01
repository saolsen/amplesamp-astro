import { For, Show } from 'solid-js';

import { useStore } from '@nanostores/solid';
import { appStore, Results, Value } from '../app';

function display(value: Value) {
    switch (value.type) {
        case 'Object': {
            return value.object_type + ':' + value.id;
            // console.log(value);
            // const fields = value.fields!;
            // let result = '{ ';
            // for (const [key, value] of fields.entries()) {
            //     result += `${key}: ${display(value)}, `;
            // }
            // return result.slice(0, -2) + ' }';
        }
        case 'Int':
            return value.value;
        case 'String':
            return value.value;
        default:
            return "todo";
    }
}

export default function Results() {
    const $appStore = useStore(appStore);

    return <div>
        <Show when={!($appStore().results)}>
            <div id="objects-tab" class="tab content1">
                Run to see results
            </div>
            <div id="output-tab" class="tab content2">
                Run to see output
            </div>
            <div id="errors-tab" class="tab content3" style="display: none">
                <h3>Errors</h3>
                <For each={Array.from($appStore().errors)}>
                    {error => <div>{JSON.stringify(error)}</div>}
                </For>
            </div>
        </Show>
        <Show when={$appStore().results}>
            <div id="objects-tab" class="tab content1">
                <h3>Objects</h3>
                <For each={Array.from($appStore().results!.types)}>
                    {typ => {
                        let object_type = typ.name;
                        console.log(object_type);
                        let values = $appStore().results!.objects.get(object_type);
                        let types = $appStore().results!.types;
                        let type = null;
                        for (let typ in types) {
                            if (types[typ].name == object_type) {
                                type = types[typ];
                                break
                            }
                        }
                        let fields = type!.field_order;
                        return <div>
                            <h4>{object_type}</h4>
                            <table>
                                <thead>
                                    <tr>
                                        <For each={Array.from(fields as string[])}>
                                            {key => <th><h4>{key}</h4></th>}
                                        </For>
                                    </tr>
                                </thead>
                                <tbody>
                                    <For each={values}>
                                        {value => <tr>
                                            <For each={Array.from(fields as string[])}>
                                                {(key) => {
                                                    let val = value.value.fields?.get(key)!;
                                                    return <td>{display(val)}</td>
                                                }}
                                            </For>
                                        </tr>}
                                    </For>
                                </tbody>
                            </table>
                        </div>
                    }
                    }
                </For>
            </div>
            <div id="output-tab" class="tab content2" style="display: none">
                <h3>Output</h3>
                <For each={Array.from($appStore().results!.output)}>
                    {output => <div>{output.value}</div>}
                </For>
            </div>
            <div id="errors-tab" class="tab content3" style="display: none">
                <h3>Errors</h3>
                <For each={Array.from($appStore().errors)}>
                    {error => <div>{JSON.stringify(error)}</div>}
                </For>
            </div>
            <div id="types-tab" class="tab content4" style="display: none">
                <h3>Types</h3>
                <For each={Array.from($appStore().results!.types)}>
                    {type => {
                        return <div>
                            <h4>{type.name}</h4>
                            <table>
                                <thead>
                                    <tr>
                                        <th><h4>Field</h4></th>
                                        <th><h4>Type</h4></th>
                                    </tr>
                                </thead>
                                <tbody>
                                    <For each={Array.from(type.field_order)}>
                                        {field_name => {
                                            let field = type.fields.get(field_name);
                                            console.log(field)
                                            return <tr>
                                                <td>{field_name as string}</td>
                                                <td>{field}</td>
                                            </tr>
                                        }}
                                    </For>
                                </tbody>
                            </table>
                        </div>
                    }}
                </For>
            </div>
            <div id="bytecode-tab" class="tab content5" style="display: none">
                <h3>Bytecode</h3>
                <For each={Array.from($appStore().results!.bytecode)}>
                    {line => <div onMouseOver={() => {
                        appStore.setKey('instruction', line.loc);
                    }}>{line.op}</div>}
                </For>
            </div>
        </Show>
    </div>
}
