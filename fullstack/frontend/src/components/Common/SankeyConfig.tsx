import { Icon, createListCollection } from "@chakra-ui/react"
import {
  Box,
  Button,
  Flex,
  HStack,
  Spinner,
  Tag,
  TagLabel,
  Text,
  VStack,
} from "@chakra-ui/react"
import { useEffect, useState } from "react"

import {
  SelectContent,
  SelectItem,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
} from "@/components/ui/select"

import { isLoggedIn } from "@/hooks/useAuth"
import { AddIcon, DeleteIcon } from "@chakra-ui/icons"
import { useMutation, useQuery } from "@tanstack/react-query"
import {
  type PossibleSankeyInput,
  type PossibleSankeyLinkage,
  SankeyService,
  type SankeySibling,
} from "../../client"
import { SankeyBox } from "./VisualizationPanel"

type Blah = { label: string; value: number }

export function SankeyConfigPage() {
  const [selectedInputs, setSelectedInputs] = useState<PossibleSankeyInput[]>(
    [],
  )
  const [selectedLinkages, setSelectedLinkages] = useState<
    PossibleSankeyLinkage[]
  >([])

  const [selectedInput, setSelectedInput] = useState<Blah | null>(null)
  const [selectedLinkage, setSelectedLinkage] = useState<Blah | null>(null)

  const { data, isLoading } = useQuery({
    queryKey: ["sankeyConfigInfo"],
    queryFn: SankeyService.getSankeyConfigInfo,
    enabled: isLoggedIn(),
  })

  const findInputFromId: Record<number, PossibleSankeyInput> | undefined =
    data?.possible_inputs.reduce(
      (acc, item) => {
        acc[item.category_id] = item
        return acc
      },
      {} as Record<number, PossibleSankeyInput>,
    )
  const findLinkageFromId:
    | Record<number, Record<number, PossibleSankeyLinkage>>
    | undefined = data?.possible_links.reduce(
    (acc, item) => {
      acc[item.category_id] = acc[item.category_id] || {}
      acc[item.category_id][item.target_source_id] = item
      return acc
    },
    {} as Record<number, Record<number, PossibleSankeyLinkage>>,
  )

  useEffect(() => {
    if (!isLoading && data) {
      setSelectedInputs(data.existing_inputs || [])
      setSelectedLinkages(data.existing_links || [])
    }
  }, [data, isLoading])

  const selectedCategoryIds = new Set(
    selectedInputs.map((input) => input.category_id),
  )

  const collectionOfInputs = {
    items:
      data?.possible_inputs?.map((input) => ({
        label: input.category_name,
        value: input.category_id,
      })) || [],
  }

  const collectionOfLinkages = {
    items:
      data?.possible_links
        ?.filter((link) => selectedCategoryIds.has(link.category_id))
        .map((link) => ({
          label: link.target_source_name,
          value: link.target_source_id,
        })) || [],
  }

  const addInput = () => {
    if (selectedInput && findInputFromId) {
      setSelectedInputs((prev) => [
        ...prev,
        findInputFromId[selectedInput.value],
      ])
      setSelectedInput(null)
    }
  }

  const addLinkage = (sibling: SankeySibling) => {
    if (selectedLinkage && findLinkageFromId) {
      setSelectedLinkages((prev) => [
        ...prev,
        findLinkageFromId[sibling.category_id][selectedLinkage.value],
      ])
      setSelectedLinkage(null)
    }
  }

  const removeInput = (inputToRemove: PossibleSankeyInput) => {
    setSelectedInputs((prev) =>
      prev.filter((input) => input.category_id !== inputToRemove.category_id),
    )
  }

  const removeLinkage = (linkToRemove: PossibleSankeyLinkage) => {
    setSelectedLinkages((prev) =>
      prev.filter((link) => link.category_id !== linkToRemove.category_id),
    )
  }

  const saveSankeyConfig = useMutation({
    mutationFn: async () => {
      const sankeyConfig = {
        requestBody: {
          inputs: selectedInputs,
          links: selectedLinkages,
        },
      }
      return SankeyService.createSankeyConfig(sankeyConfig)
    },
    onSuccess: () => {
      alert("Sankey Configuration Saved!")
    },
    onError: (err) => {
      console.error(err)
      alert("Error saving configuration")
    },
  })

  const getLinkage = (sibling: SankeySibling): PossibleSankeyLinkage | null => {
    const linkage = selectedLinkages.find(
      (l) => l.category_id === sibling.category_id,
    )
    return linkage ? linkage : null
  }

  if (isLoading) {
    return <Spinner />
  }

  return (
    <Flex direction="column" p={4} h="100vh">
      <VStack align="start" spaceX={4} width="100%">
        {selectedInputs.map((input, index) => (
          <div key={index.toString()}>
            <Box
              key={input.category_id}
              px={2}
              py={1}
              borderRadius="md"
              spaceX={2}
              ml={4}
            >
              <HStack>
                <SimpleTag
                  name={input.category_name}
                  onRemove={() => removeInput(input)}
                  showRemove={true}
                  showAdd={false}
                  onAdd={() => {}}
                />
              </HStack>
              {findInputFromId && (
                <VStack align={"flex-start"} ml={4}>
                  <SimpleTag name={input.source_name} />
                  <Box ml={4}>
                    {findInputFromId[input.category_id as number].siblings.map(
                      (sibling: SankeySibling, inner) => (
                        <LinkageDisplay
                          key={inner.toString()}
                          sibling={sibling}
                          addLinkage={addLinkage}
                          removeLinkage={removeLinkage}
                          linkage={getLinkage(sibling)}
                          setSelectedLinkage={setSelectedLinkage}
                          collectionOfLinkages={collectionOfLinkages}
                        />
                      ),
                    )}
                  </Box>
                </VStack>
              )}
            </Box>
          </div>
        ))}
        <Selector
          collection={collectionOfInputs}
          selected={null}
          setSelected={setSelectedInput}
          onAdd={addInput}
          title="Input"
        />
        <Button
          colorScheme="blue"
          onClick={() => saveSankeyConfig.mutate()}
          disabled={
            selectedInputs.length === 0 && selectedLinkages.length === 0
          }
        >
          Save Configuration
        </Button>
      </VStack>
      <SankeyBox />
    </Flex>
  )
}

interface LinkageDisplayProps {
  sibling: SankeySibling
  linkage: PossibleSankeyLinkage | null
  addLinkage: (sibling: SankeySibling) => void
  removeLinkage: (linkage: PossibleSankeyLinkage) => void
  setSelectedLinkage: React.Dispatch<React.SetStateAction<Blah | null>>
  collectionOfLinkages: { items: Blah[] }
}

const LinkageDisplay: React.FC<LinkageDisplayProps> = ({
  sibling,
  addLinkage,
  removeLinkage,
  collectionOfLinkages,
  setSelectedLinkage,
  linkage,
}) => {
  const [showSelector, setShowSelector] = useState(false)

  return (
    <HStack key={sibling.category_name} ml={4}>
      {linkage ? (
        <>
          <SimpleTag
            name={sibling.category_name}
            onAdd={() => addLinkage(sibling)}
          />
          <Text>has linkage to</Text>
          <SimpleTag
            name={linkage.target_source_name!}
            onRemove={() => {
              removeLinkage(linkage)
              setShowSelector(false)
            }}
            showRemove
          />
        </>
      ) : (
        <>
          <SimpleTag
            name={sibling.category_name}
            showAdd={!showSelector}
            onAdd={() => setShowSelector(true)}
          />
          {showSelector && (
            <Selector
              collection={collectionOfLinkages}
              selected={null}
              setSelected={setSelectedLinkage}
              onAdd={() => addLinkage(sibling)}
              onRemove={() => setShowSelector(false)}
              title="Link to"
            />
          )}
        </>
      )}
    </HStack>
  )
}

interface SimpleTagProps {
  name: string
  onRemove?: () => void
  onAdd?: () => void
  showRemove?: boolean
  showAdd?: boolean
}

function SimpleTag({
  name,
  onRemove,
  onAdd,
  showRemove = false,
  showAdd = false,
}: SimpleTagProps) {
  return (
    <Tag.Root key={name} p={2} borderRadius="md">
      <TagLabel>{name}</TagLabel>
      {showRemove && <Icon as={DeleteIcon} mr={1} onClick={onRemove} />}
      {showAdd && <Icon as={AddIcon} mr={1} onClick={onAdd} />}
    </Tag.Root>
  )
}

function Selector(props: {
  collection: { items: Blah[] }
  selected: Blah | null
  setSelected: (selected: Blah | null) => void
  onAdd?: () => void
  onRemove?: () => void
  title: string
}) {
  return (
    <HStack>
      <SelectRoot
        onValueChange={(selectedItems) => {
          if (selectedItems.items[0]) {
            props.setSelected(selectedItems.items[0])
          }
        }}
        collection={createListCollection(props.collection)}
        size="sm"
        width="320px"
      >
        <SelectTrigger>
          <SelectValueText placeholder={props.title} />
        </SelectTrigger>
        <SelectContent>
          {props.collection.items.map((item) => (
            <SelectItem item={item} key={item.value}>
              {item.label}
            </SelectItem>
          ))}
        </SelectContent>
      </SelectRoot>
      <SimpleTag
        name={""}
        showAdd={!!props.onAdd}
        onAdd={props.onAdd}
        showRemove={!!props.onRemove}
        onRemove={props.onRemove}
      />
    </HStack>
  )
}
